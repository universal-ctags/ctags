/*
 *  MIO, an I/O abstraction layer replicating C file I/O API.
 *  Copyright (C) 2010  Colomban Wendling <ban@herbesfolles.org>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with this program; if not, write to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 */

#ifndef READTAGS_DSL
#include "general.h"  /* must always come first */

#include "routines.h"
#include "debug.h"
#else

#if defined (HAVE_CONFIG_H)
#include <config.h>
#endif

#ifdef HAVE_STDBOOL_H
#include <stdbool.h>
#endif
#endif	/* READTAGS_DSL */

#include "mio.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>

#ifndef _MSC_VER
#define MAY_HAVE_FTRUNCATE
#include <unistd.h>
#endif

#ifdef READTAGS_DSL
#define xMalloc(n,Type)    (Type *)eMalloc((size_t)(n) * sizeof (Type))
#define xRealloc(p,n,Type) (Type *)eRealloc((p), (n) * sizeof (Type))

static void *eMalloc (const size_t size)
{
	void *buffer = malloc (size);

	if (buffer == NULL)
	{
		fprintf(stderr, "out of memory");
		abort ();
	}

	return buffer;
}

static void *eRealloc (void *const ptr, const size_t size)
{
	void *buffer;
	if (ptr == NULL)
		buffer = eMalloc (size);
	else
	{
		buffer = realloc (ptr, size);
		if (buffer == NULL)
		{
			fprintf(stderr, "out of memory");
			abort ();
		}
	}
	return buffer;
}

static void eFree (void *const ptr)
{
	free (ptr);
}
#define eFreeNoNullCheck eFree

#  define Assert(c) do {} while(0)
#  define AssertNotReached() do {} while(0)
#endif	/* READTAGS_DSL */

/* minimal reallocation chunk size */
#define MIO_CHUNK_SIZE 4096

#define MAX(a, b)  (((a) > (b)) ? (a) : (b))


/**
 * SECTION:mio
 * @short_description: The MIO object
 * @include: mio/mio.h
 *
 * The #MIO object replicates the C file I/O API with support of both standard
 * file based operations and in-memory operations. Its goal is to ease the port
 * of an application that uses C file I/O API to perform in-memory operations.
 *
 * A #MIO object is created using mio_new_file(), mio_new_memory() or mio_new_mio(),
 * depending on whether you want file or in-memory operations.
 * Its life is managed by reference counting. Just after calling one of functions
 * for creating, the count is 1. mio_ref() increments the counter. mio_unref()
 * decrements it. When the counter becomes 0, the #MIO object will be destroyed
 * in mio_unref(). There is also some other convenient API to create file-based
 * #MIO objects for more complex cases, such as mio_new_file_full() and
 * mio_new_fp().
 *
 * Once the #MIO object is created, you can perform standard I/O operations on
 * it transparently without the need to care about the effective underlying
 * operations.
 *
 * The I/O API is almost exactly a replication of the standard C file I/O API
 * with the significant difference that the first parameter is always the #MIO
 * object to work on.
 */


typedef struct _MIOUserData MIOUserData;
struct _MIOUserData {
	void *d;
	MIODestroyNotify f;
};

/**
 * MIO:
 *
 * An object representing a #MIO stream. No assumptions should be made about
 * what compose this object, and none of its fields should be accessed directly.
 */
struct _MIO {
	/*< private >*/
	MIOType type;
	unsigned int refcount;
	union {
		struct {
			FILE *fp;
			MIOFCloseFunc close_func;
		} file;
		struct {
			unsigned char *buf;
			int ungetch;
			size_t pos;
			size_t size;
			size_t allocated_size;
			MIOReallocFunc realloc_func;
			MIODestroyNotify free_func;
			bool error;
			bool eof;
		} mem;
	} impl;
	MIOUserData udata;
};


/**
 * mio_new_file_full:
 * @filename: Filename to open, passed as-is to @open_func as the first argument
 * @mode: Mode in which open the file, passed as-is to @open_func as the second
 *        argument
 * @open_func: A function with the fopen() semantic to use to open the file
 * @close_func: A function with the fclose() semantic to close the file when
 *              the #MIO object is destroyed, or %NULL not to close the #FILE
 *              object
 *
 * Creates a new #MIO object working on a file, from a filename and an opening
 * function. See also mio_new_file().
 *
 * This function is generally overkill and mio_new_file() should often be used
 * instead, but it allows to specify a custom function to open a file, as well
 * as a close function. The former is useful e.g. if you need to wrap fopen()
 * for some reason (like filename encoding conversion for example), and the
 * latter allows you both to match your custom open function and to choose
 * whether the underlying #FILE object should or not be closed when mio_unref()
 * is called on the returned object.
 *
 * Free-function: mio_unref()
 *
 * Returns: A new #MIO on success, or %NULL on failure.
 */
MIO *mio_new_file_full (const char *filename,
						const char *mode,
						MIOFOpenFunc open_func,
						MIOFCloseFunc close_func)
{
	MIO *mio;

	/* we need to create the MIO object first, because we may not be able to close
	 * the opened file if the user passed NULL as the close function, which means
	 * that everything must succeed if we've opened the file successfully */
	mio = xMalloc (1, MIO);
	if (mio)
	{
		FILE *fp = open_func (filename, mode);

		if (! fp)
		{
			eFree (mio);
			mio = NULL;
		}
		else
		{
			mio->type = MIO_TYPE_FILE;
			mio->impl.file.fp = fp;
			mio->impl.file.close_func = close_func;
			mio->refcount = 1;
			mio->udata.d = NULL;
			mio->udata.f = NULL;
		}
	}

	return mio;
}

/**
 * mio_new_file:
 * @filename: Filename to open, same as the fopen()'s first argument
 * @mode: Mode in which open the file, fopen()'s second argument
 *
 * Creates a new #MIO object working on a file from a filename; wrapping
 * fopen().
 * This function simply calls mio_new_file_full() with the libc's fopen() and
 * fclose() functions.
 *
 * Free-function: mio_unref()
 *
 * Returns: A new #MIO on success, or %NULL on failure.
 */
MIO *mio_new_file (const char *filename, const char *mode)
{
	return mio_new_file_full (filename, mode, fopen, fclose);
}

/**
 * mio_new_fp:
 * @fp: An opened #FILE object
 * @close_func: (allow-none): Function used to close @fp when the #MIO object
 *              gets destroyed, or %NULL not to close the #FILE object
 *
 * Creates a new #MIO object working on a file, from an already opened #FILE
 * object.
 *
 * <example>
 * <title>Typical use of this function</title>
 * <programlisting>
 * MIO *mio = mio_new_fp (fp, fclose);
 * </programlisting>
 * </example>
 *
 * Free-function: mio_unref()
 *
 * Returns: A new #MIO on success or %NULL on failure.
 */
MIO *mio_new_fp (FILE *fp, MIOFCloseFunc close_func)
{
	MIO *mio;

	if (!fp)
		return NULL;

	mio = xMalloc (1, MIO);
	if (mio)
	{
		mio->type = MIO_TYPE_FILE;
		mio->impl.file.fp = fp;
		mio->impl.file.close_func = close_func;
		mio->refcount = 1;
		mio->udata.d = NULL;
		mio->udata.f = NULL;
	}

	return mio;
}

/**
 * mio_new_memory:
 * @data: Initial data (may be %NULL)
 * @size: Length of @data in bytes
 * @realloc_func: A function with the realloc() semantic used to grow the
 *                buffer, or %NULL to disable buffer growing
 * @free_func: A function with the free() semantic to destroy the data together
 *             with the object, or %NULL not to destroy the data
 *
 * Creates a new #MIO object working on memory.
 *
 * To allow the buffer to grow, you must provide a @realloc_func, otherwise
 * trying to write after the end of the current data will fail.
 *
 * If you want the buffer to be freed together with the #MIO object, you must
 * give a @free_func; otherwise the data will still live after #MIO object
 * termination.
 *
 * <example>
 * <title>Basic creation of a non-growable, freeable #MIO object</title>
 * <programlisting>
 * MIO *mio = mio_new_memory (data, size, NULL, g_free);
 * </programlisting>
 * </example>
 *
 * <example>
 * <title>Basic creation of an empty growable and freeable #MIO object</title>
 * <programlisting>
 * MIO *mio = mio_new_memory (NULL, 0, g_try_realloc, g_free);
 * </programlisting>
 * </example>
 *
 * Free-function: mio_unref()
 *
 * Returns: A new #MIO on success, or %NULL on failure.
 */
MIO *mio_new_memory (unsigned char *data,
					 size_t size,
					 MIOReallocFunc realloc_func,
					 MIODestroyNotify free_func)
{
	MIO *mio;

	mio = xMalloc (1, MIO);
	if (mio)
	{
		mio->type = MIO_TYPE_MEMORY;
		mio->impl.mem.buf = data;
		mio->impl.mem.ungetch = EOF;
		mio->impl.mem.pos = 0;
		mio->impl.mem.size = size;
		mio->impl.mem.allocated_size = size;
		mio->impl.mem.realloc_func = realloc_func;
		mio->impl.mem.free_func = free_func;
		mio->impl.mem.eof = false;
		mio->impl.mem.error = false;
		mio->refcount = 1;
		mio->udata.d = NULL;
		mio->udata.f = NULL;
	}

	return mio;
}

/**
 * mio_new_mio:
 * @base: The original mio
 * @start: stream offset of the @base where new mio starts
 * @size: the length of the data copied from @base to new mio
 *
 * Creates a new #MIO object by copying data from existing #MIO (@base).
 * The range for copying is given with @start and @size.
 * Copying data at the range from @start to the end of @base is
 * done if -1 is given as @size.
 *
 * If @size is larger than the length from @start to the end of
 * @base, %NULL is returned.
 *
 * The function doesn't move the file position of @base.
 *
 * Free-function: mio_unref()
 *
 */

MIO *mio_new_mio (MIO *base, long start, long size)
{
	unsigned char *data;
	long original_pos;
	MIO *submio;
	size_t r;

	original_pos = mio_tell (base);

	if (size == -1)
	{
		long end;

		if (mio_seek (base, 0, SEEK_END) != 0)
			return NULL;
		end = mio_tell (base);
		Assert (end >= start);
		size = end - start;
	}

	if (mio_seek (base, start, SEEK_SET) != 0)
		return NULL;

	data = xMalloc (size, unsigned char);
	r= mio_read (base, data, 1, size);
	mio_seek (base, original_pos, SEEK_SET);

	if (r != size)
		goto cleanup;

	submio = mio_new_memory (data, size, eRealloc, eFreeNoNullCheck);
	if (! submio)
		goto cleanup;

	return submio;

cleanup:
	eFree (data);
	return NULL;
}

/**
 * mio_ref:
 * @mio: A #MIO object
 *
 * Increments the reference counter of a #MIO.
 *
 * Returns: passed @mio.
 */
MIO *mio_ref        (MIO *mio)
{
	mio->refcount++;
	return mio;
}

/**
 * mio_file_get_fp:
 * @mio: A #MIO object
 *
 * Gets the underlying #FILE object associated with a #MIO file stream.
 *
 * <warning><para>The returned object may become invalid after a call to
 * mio_unref() if the stream was configured to close the file when
 * destroyed.</para></warning>
 *
 * Returns: The underlying #FILE object of the given stream, or %NULL if the
 *          stream is not a file stream.
 */
FILE *mio_file_get_fp (MIO *mio)
{
	FILE *fp = NULL;

	if (mio->type == MIO_TYPE_FILE)
		fp = mio->impl.file.fp;

	return fp;
}

/**
 * mio_memory_get_data:
 * @mio: A #MIO object
 * @size: (allow-none) (out): Return location for the length of the returned
 *        memory, or %NULL
 *
 * Gets the underlying memory buffer associated with a #MIO memory stream.
 *
 * <warning><para>The returned pointer and size may become invalid after a
 * successful write on the stream or after a call to mio_unref() if the stream
 * was configured to free the memory when destroyed.</para></warning>
 *
 * Returns: The memory buffer of the given #MIO stream, or %NULL if the stream
 *          is not a memory stream.
 */
unsigned char *mio_memory_get_data (MIO *mio, size_t *size)
{
	unsigned char *ptr = NULL;

	if (mio->type == MIO_TYPE_MEMORY)
	{
		ptr = mio->impl.mem.buf;
		if (size)
			*size = mio->impl.mem.size;
	}

	return ptr;
}

/**
 * mio_unref:
 * @mio: A #MIO object
 *
 * Decrements the reference counter of a #MIO and destroys the #MIO
 * object if its counter becomes 0.
 *
 * Returns: Error code obtained from the registered MIOFCloseFunc or 0 on success.
 */
int mio_unref (MIO *mio)
{
	int rv = 0;

	if (mio)
	{
		if (--mio->refcount)
			return 0;

		if (mio->udata.d && mio->udata.f)
			mio->udata.f (mio->udata.d);

		if (mio->type == MIO_TYPE_FILE)
		{
			if (mio->impl.file.close_func)
				rv = mio->impl.file.close_func (mio->impl.file.fp);
			mio->impl.file.close_func = NULL;
			mio->impl.file.fp = NULL;
		}
		else if (mio->type == MIO_TYPE_MEMORY)
		{
			if (mio->impl.mem.free_func)
				mio->impl.mem.free_func (mio->impl.mem.buf);
			mio->impl.mem.buf = NULL;
			mio->impl.mem.pos = 0;
			mio->impl.mem.size = 0;
			mio->impl.mem.allocated_size = 0;
			mio->impl.mem.realloc_func = NULL;
			mio->impl.mem.free_func = NULL;
			mio->impl.mem.eof = false;
			mio->impl.mem.error = false;
		}
		else
			AssertNotReached ();

		eFree (mio);
	}

	return rv;
}

/**
 * mio_read:
 * @mio: A #MIO object
 * @ptr: Pointer to the memory to fill with the read data
 * @size: Size of each block to read
 * @nmemb: Number o blocks to read
 *
 * Reads raw data from a #MIO stream. This function behave the same as fread().
 *
 * Returns: The number of actually read blocks. If an error occurs or if the
 *          end of the stream is reached, the return value may be smaller than
 *          the requested block count, or even 0. This function doesn't
 *          distinguish between end-of-stream and an error, you should then use
 *          mio_eof() and mio_error() to determine which occurred.
 */
size_t mio_read (MIO *mio,
				 void *ptr_,
				 size_t size,
				 size_t nmemb)
{
	if (mio->type == MIO_TYPE_FILE)
		return fread (ptr_, size, nmemb, mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		size_t n_read = 0;

		if (size != 0 && nmemb != 0)
		{
			size_t size_avail = mio->impl.mem.size - mio->impl.mem.pos;
			size_t copy_bytes = size * nmemb;
			unsigned char *ptr = ptr_;

			if (size_avail < copy_bytes)
				copy_bytes = size_avail;

			if (copy_bytes > 0)
			{
				n_read = copy_bytes / size;

				if (mio->impl.mem.ungetch != EOF)
				{
					*ptr = (unsigned char) mio->impl.mem.ungetch;
					mio->impl.mem.ungetch = EOF;
					copy_bytes--;
					mio->impl.mem.pos++;
					ptr++;
				}

				memcpy (ptr, &mio->impl.mem.buf[mio->impl.mem.pos], copy_bytes);
				mio->impl.mem.pos += copy_bytes;
			}
			if (mio->impl.mem.pos >= mio->impl.mem.size)
				mio->impl.mem.eof = true;
		}

		return n_read;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/*
 * mem_try_resize:
 * @mio: A #MIO object of the type %MIO_TYPE_MEMORY
 * @new_size: Requested new size
 *
 * Tries to resize the underlying buffer of an in-memory #MIO object.
 * This supports both growing and shrinking.
 *
 * Returns: %true on success, %false otherwise.
 */
static int mem_try_resize (MIO *mio, size_t new_size)
{
	int success = false;

	if (mio->impl.mem.realloc_func)
	{
		if (new_size == ULONG_MAX)
		{
#ifdef EOVERFLOW
			errno = EOVERFLOW;
#endif
		}
		else
		{
			if (new_size > mio->impl.mem.size)
			{
				if (new_size <= mio->impl.mem.allocated_size)
				{
					mio->impl.mem.size = new_size;
					success = true;
				}
				else
				{
					size_t newsize;
					unsigned char *newbuf;

					newsize = MAX (mio->impl.mem.allocated_size + MIO_CHUNK_SIZE,
								   new_size);
					newbuf = mio->impl.mem.realloc_func (mio->impl.mem.buf, newsize);
					if (newbuf)
					{
						mio->impl.mem.buf = newbuf;
						mio->impl.mem.allocated_size = newsize;
						mio->impl.mem.size = new_size;
						success = true;
					}
				}
			}
			else
			{
				unsigned char *newbuf;

				newbuf = mio->impl.mem.realloc_func (mio->impl.mem.buf, new_size);
				if (newbuf || new_size == 0)
				{
					mio->impl.mem.buf = newbuf;
					mio->impl.mem.allocated_size = new_size;
					mio->impl.mem.size = new_size;
					success = true;
				}
			}
		}
	}

	return success;
}

static int file_try_resize (MIO *mio, size_t new_size)
{
	mio_flush (mio);

	FILE *fp = mio_file_get_fp (mio);

#ifdef MAY_HAVE_FTRUNCATE
	if (ftruncate(fileno(fp), (off_t)new_size) < 0)
		return false;
#else
	/* See https://stackoverflow.com/questions/873454/how-to-truncate-a-file-in-c/874704#874704 */
	if ((errno = _chsize_s(_fileno(fp), (__int64)new_size)) != 0)
		return false;
#endif
	return true;
}

int mio_try_resize (MIO *mio, size_t new_size)
{
	if (mio->type == MIO_TYPE_MEMORY)
		return mem_try_resize (mio, new_size);
	else
		return file_try_resize (mio, new_size);
}

/*
 * mem_try_ensure_space:
 * @mio: A #MIO object
 * @n: Requested size from the current (cursor) position
 *
 * Tries to ensure there is enough space for @n bytes to be written from the
 * current cursor position.
 *
 * Returns: %true if there is enough space, %false otherwise.
 */
static int mem_try_ensure_space (MIO *mio, size_t n)
{
	int success = true;

	if (mio->impl.mem.pos + n > mio->impl.mem.size)
		success = mem_try_resize (mio, mio->impl.mem.pos + n);

	return success;
}

/**
 * mio_write:
 * @mio: A #MIO object
 * @ptr: Pointer to the memory to write on the stream
 * @size: Size of each block to write
 * @nmemb: Number of block to write
 *
 * Writes raw data to a #MIO stream. This function behaves the same as fwrite().
 *
 * Returns: The number of blocks actually written to the stream. This might be
 *          smaller than the requested count if a write error occurs.
 */
size_t mio_write (MIO *mio,
				  const void *ptr,
				  size_t size,
				  size_t nmemb)
{
	if (mio->type == MIO_TYPE_FILE)
		return fwrite (ptr, size, nmemb, mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		size_t n_written = 0;

		if (size != 0 && nmemb != 0)
		{
			if (mem_try_ensure_space (mio, size * nmemb))
			{
				memcpy (&mio->impl.mem.buf[mio->impl.mem.pos], ptr, size * nmemb);
				mio->impl.mem.pos += size * nmemb;
				n_written = nmemb;
			}
		}

		return n_written;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_putc:
 * @mio: A #MIO object
 * @c: The character to write
 *
 * Writes a character to a #MIO stream. This function behaves the same as
 * fputc().
 *
 * Returns: The written character, or %EOF on error.
 */
int mio_putc (MIO *mio, int c)
{
	if (mio->type == MIO_TYPE_FILE)
		return fputc (c, mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		int rv = EOF;

		if (mem_try_ensure_space (mio, 1))
		{
			mio->impl.mem.buf[mio->impl.mem.pos] = (unsigned char)c;
			mio->impl.mem.pos++;
			rv = (int)((unsigned char)c);
		}

		return rv;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_puts:
 * @mio: A #MIO object
 * @s: The string to write
 *
 * Writes a string to a #MIO object. This function behaves the same as fputs().
 *
 * Returns: A non-negative integer on success or %EOF on failure.
 */
int mio_puts (MIO *mio, const char *s)
{
	if (mio->type == MIO_TYPE_FILE)
		return fputs (s, mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		int rv = EOF;
		size_t len;

		len = strlen (s);
		if (mem_try_ensure_space (mio, len))
		{
			memcpy (&mio->impl.mem.buf[mio->impl.mem.pos], s, len);
			mio->impl.mem.pos += len;
			rv = 1;
		}

		return rv;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_vprintf:
 * @mio: A #MIO object
 * @format: A printf format string
 * @ap: The variadic argument list for the format
 *
 * Writes a formatted string into a #MIO stream. This function behaves the same
 * as vfprintf().
 *
 * Returns: The number of bytes written in the stream, or a negative value on
 *          failure.
 */
int mio_vprintf (MIO *mio, const char *format, va_list ap)
{
	if (mio->type == MIO_TYPE_FILE)
		return vfprintf (mio->impl.file.fp, format, ap);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		int rv = -1;
		size_t n;
		size_t old_pos;
		size_t old_size;
		va_list ap_copy;
		char dummy;

		old_pos = mio->impl.mem.pos;
		old_size = mio->impl.mem.size;
		va_copy (ap_copy, ap);
		/* compute the size we will need into the buffer */
		n = vsnprintf (&dummy, 1, format, ap_copy) + 1;
		va_end (ap_copy);
		if (mem_try_ensure_space (mio, n))
		{
			unsigned char c;

			/* backup character at n+1 that will be overwritten by a \0 ... */
			c = mio->impl.mem.buf[mio->impl.mem.pos + (n - 1)];
			rv = vsprintf ((char *)&mio->impl.mem.buf[mio->impl.mem.pos], format, ap);
			/* ...and restore it */
			mio->impl.mem.buf[mio->impl.mem.pos + (n - 1)] = c;
			if (rv >= 0 && (size_t)rv == (n - 1))
			{
				/* re-compute the actual size since we might have allocated one byte
				 * more than needed */
				mio->impl.mem.size = MAX (old_size, old_pos + (unsigned int)rv);
				mio->impl.mem.pos += (unsigned int)rv;
			}
			else
			{
				mio->impl.mem.size = old_size;
				rv = -1;
			}
		}

		return rv;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_printf:
 * @mio: A #MIO object
 * @format: A print format string
 * @...: Arguments of the format
 *
 * Writes a formatted string to a #MIO stream. This function behaves the same as
 * fprintf().
 *
 * Returns: The number of bytes written to the stream, or a negative value on
 *          failure.
 */
int mio_printf (MIO *mio, const char *format, ...)
{
	int rv;
	va_list ap;

	va_start (ap, format);
	rv = mio_vprintf (mio, format, ap);
	va_end (ap);

	return rv;
}

/**
 * mio_getc:
 * @mio: A #MIO object
 *
 * Gets the current character from a #MIO stream. This function behaves the same
 * as fgetc().
 *
 * Returns: The read character as a #int, or %EOF on error.
 */
int mio_getc (MIO *mio)
{
	if (mio->type == MIO_TYPE_FILE)
		return fgetc (mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		int rv = EOF;

		if (mio->impl.mem.ungetch != EOF)
		{
			rv = mio->impl.mem.ungetch;
			mio->impl.mem.ungetch = EOF;
			mio->impl.mem.pos++;
		}
		else if (mio->impl.mem.pos < mio->impl.mem.size)
		{
			rv = mio->impl.mem.buf[mio->impl.mem.pos];
			mio->impl.mem.pos++;
		}
		else
			mio->impl.mem.eof = true;

		return rv;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_ungetc:
 * @mio: A #MIO object
 * @ch: Character to put back in the stream
 *
 * Puts a character back in a #MIO stream. This function behaves the sames as
 * ungetc().
 *
 * <warning><para>It is only guaranteed that one character can be but back at a
 * time, even if the implementation may allow more.</para></warning>
 * <warning><para>Using this function while the stream cursor is at offset 0 is
 * not guaranteed to function properly. As the C99 standard says, it is "an
 * obsolescent feature".</para></warning>
 *
 * Returns: The character put back, or %EOF on error.
 */
int mio_ungetc (MIO *mio, int ch)
{
	if (mio->type == MIO_TYPE_FILE)
		return ungetc (ch, mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		int rv = EOF;

		if (ch != EOF && mio->impl.mem.ungetch == EOF)
		{
			rv = mio->impl.mem.ungetch = ch;
			mio->impl.mem.pos--;
			mio->impl.mem.eof = false;
		}

		return rv;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_gets:
 * @mio: A #MIO object
 * @s: A string to fill with the read data
 * @size: The maximum number of bytes to read
 *
 * Reads a string from a #MIO stream, stopping after the first new-line
 * character or at the end of the stream. This function behaves the same as
 * fgets().
 *
 * Returns: @s on success, %NULL otherwise.
 */
char *mio_gets (MIO *mio, char *s, size_t size)
{
	if (mio->type == MIO_TYPE_FILE)
		return fgets (s, (int)size, mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		char *rv = NULL;

		if (size > 0)
		{
			size_t i = 0;
			bool newline = false;
			/* Avoiding dereference inside the for loop below improves
			 * performance so we do it here. */
			size_t pos = mio->impl.mem.pos;
			size_t buf_size = mio->impl.mem.size;
			unsigned char *buf = mio->impl.mem.buf;

			if (mio->impl.mem.ungetch != EOF)
			{
				s[i] = (char)mio->impl.mem.ungetch;
				mio->impl.mem.ungetch = EOF;
				pos++;
				i++;
			}
			for (; pos < buf_size && i < (size - 1); i++)
			{
				s[i] = (char)buf[pos];
				pos++;
				if (s[i] == '\n')
				{
					i++;
					newline = true;
					break;
				}
			}
			if (i > 0)
			{
				s[i] = 0;
				rv = s;
			}
			if (!newline && pos >= buf_size)
				mio->impl.mem.eof = true;
			mio->impl.mem.pos = pos;
			mio->impl.mem.size = buf_size;
		}

		return rv;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_clearerr:
 * @mio: A #MIO object
 *
 * Clears the error and end-of-stream indicators of a #MIO stream. This function
 * behaves the same as clearerr().
 */
void mio_clearerr (MIO *mio)
{
	if (mio->type == MIO_TYPE_FILE)
		clearerr (mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		mio->impl.mem.error = false;
		mio->impl.mem.eof = false;
	}
	else
		AssertNotReached ();
}

/**
 * mio_eof:
 * @mio: A #MIO object
 *
 * Checks whether the end-of-stream indicator of a #MIO stream is set. This
 * function behaves the same as feof().
 *
 * Returns: A non-null value if the stream reached its end, 0 otherwise.
 */
int mio_eof (MIO *mio)
{
	if (mio->type == MIO_TYPE_FILE)
		return feof (mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
		return mio->impl.mem.eof != false;
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_error:
 * @mio: A #MIO object
 *
 * Checks whether the error indicator of a #MIO stream is set. This function
 * behaves the same as ferror().
 *
 * Returns: A non-null value if the stream have an error set, 0 otherwise.
 */
int mio_error (MIO *mio)
{
	if (mio->type == MIO_TYPE_FILE)
		return ferror (mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
		return mio->impl.mem.error != false;
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_seek:
 * @mio: A #MIO object
 * @offset: Offset of the new place, from @whence
 * @whence: Move origin. SEEK_SET moves relative to the start of the stream,
 *          SEEK_CUR from the current position and SEEK_SET from the end of the
 *          stream.
 *
 * Sets the cursor position on a #MIO stream. This functions behaves the same as
 * fseek(). See also mio_tell() and mio_setpos().
 *
 * Returns: 0 on success, -1 otherwise, in which case errno should be set to
 *          indicate the error.
 */
int mio_seek (MIO *mio, long offset, int whence)
{
	if (mio->type == MIO_TYPE_FILE)
		return fseek (mio->impl.file.fp, offset, whence);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		/* FIXME: should we support seeking out of bounds like lseek() seems to do? */
		int rv = -1;

		switch (whence)
		{
			case SEEK_SET:
				if (offset < 0 || (size_t)offset > mio->impl.mem.size)
					errno = EINVAL;
				else
				{
					mio->impl.mem.pos = (size_t)offset;
					rv = 0;
				}
				break;

			case SEEK_CUR:
				if ((offset < 0 && (size_t)-offset > mio->impl.mem.pos) ||
					mio->impl.mem.pos + (size_t)offset > mio->impl.mem.size)
				{
					errno = EINVAL;
				}
				else
				{
					mio->impl.mem.pos = (size_t)(mio->impl.mem.pos + offset);
					rv = 0;
				}
				break;

			case SEEK_END:
				if (offset > 0 || (size_t)-offset > mio->impl.mem.size)
					errno = EINVAL;
				else
				{
					mio->impl.mem.pos = mio->impl.mem.size - (size_t)-offset;
					rv = 0;
				}
				break;

			default:
				errno = EINVAL;
		}
		if (rv == 0)
		{
			mio->impl.mem.eof = false;
			mio->impl.mem.ungetch = EOF;
		}

		return rv;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}

}

/**
 * mio_tell:
 * @mio: A #MIO object
 *
 * Gets the current cursor position of a #MIO stream. This function behaves the
 * same as ftell().
 *
 * Returns: The current offset from the start of the stream, or -1 or error, in
 *          which case errno is set to indicate the error.
 */
long mio_tell (MIO *mio)
{
	if (mio->type == MIO_TYPE_FILE)
		return ftell (mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		long rv = -1;

		if (mio->impl.mem.pos > LONG_MAX)
		{
#ifdef EOVERFLOW
			errno = EOVERFLOW;
#endif
		}
		else
			rv = (long)mio->impl.mem.pos;

		return rv;
	}
	else
	{
		AssertNotReached ();
		return 0;
	}
}

/**
 * mio_rewind:
 * @mio: A #MIO object
 *
 * Resets the cursor position to 0, and also the end-of-stream and the error
 * indicators of a #MIO stream.
 * See also mio_seek() and mio_clearerr().
 */
void mio_rewind (MIO *mio)
{
	if (mio->type == MIO_TYPE_FILE)
		rewind (mio->impl.file.fp);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		mio->impl.mem.pos = 0;
		mio->impl.mem.ungetch = EOF;
		mio->impl.mem.eof = false;
		mio->impl.mem.error = false;
	}
	else
		AssertNotReached ();
}

/**
 * mio_getpos:
 * @mio: A #MIO stream
 * @pos: (out): A #MIOPos object to fill-in
 *
 * Stores the current position (and maybe other informations about the stream
 * state) of a #MIO stream in order to restore it later with mio_setpos(). This
 * function behaves the same as fgetpos().
 *
 * Returns: 0 on success, -1 otherwise, in which case errno is set to indicate
 *          the error.
 */
int mio_getpos (MIO *mio, MIOPos *pos)
{
	int rv = -1;

	pos->type = mio->type;
	if (mio->type == MIO_TYPE_FILE)
		rv = fgetpos (mio->impl.file.fp, &pos->impl.file);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		rv = -1;

		if (mio->impl.mem.pos == (size_t)-1)
		{
			/* this happens if ungetc() was called at the start of the stream */
#ifdef EIO
			errno = EIO;
#endif
		}
		else
		{
			pos->impl.mem = mio->impl.mem.pos;
			rv = 0;
		}
	}
	else
		AssertNotReached();

#ifdef MIO_DEBUG
	if (rv != -1)
	{
		pos->tag = mio;
	}
#endif /* MIO_DEBUG */

	return rv;
}

/**
 * mio_setpos:
 * @mio: A #MIO object
 * @pos: (in): A #MIOPos object filled-in by a previous call of mio_getpos() on
 *       the same stream
 *
 * Restores the position and state indicators of a #MIO stream previously saved
 * by mio_getpos().
 *
 * <warning><para>The #MIOPos object must have been initialized by a previous
 * call to mio_getpos() on the same stream.</para></warning>
 *
 * Returns: 0 on success, -1 otherwise, in which case errno is set to indicate
 *          the error.
 */
int mio_setpos (MIO *mio, MIOPos *pos)
{
	int rv = -1;

#ifdef MIO_DEBUG
	if (pos->tag != mio)
	{
		g_critical ("mio_setpos((MIO*)%p, (MIOPos*)%p): "
					"Given MIOPos was not set by a previous call to mio_getpos() "
					"on the same MIO object, which means there is a bug in "
					"someone's code.",
					(void *)mio, (void *)pos);
		errno = EINVAL;
		return -1;
	}
#endif /* MIO_DEBUG */

	if (mio->type == MIO_TYPE_FILE)
		rv = fsetpos (mio->impl.file.fp, &pos->impl.file);
	else if (mio->type == MIO_TYPE_MEMORY)
	{
		rv = -1;

		if (pos->impl.mem > mio->impl.mem.size)
			errno = EINVAL;
		else
		{
			mio->impl.mem.ungetch = EOF;
			mio->impl.mem.pos = pos->impl.mem;
			rv = 0;
		}
	}
	else
		AssertNotReached ();

	return rv;
}

/**
 * mio_flush:
 * @mio: A #MIO object
 *
 * Forces a write of all user-space buffered data for the given output or update
 * stream via the stream's underlying write function. Only applicable when using
 * FILE back-end.
 *
 * Returns: 0 on success, error code otherwise.
 */
int mio_flush (MIO *mio)
{
	if (mio->type == MIO_TYPE_FILE)
		return fflush (mio->impl.file.fp);
	return 0;
}


/**
 * mio_attach_user_data:
 * @mio: A #MIO object
 * @user_data: a pointer to any data object
 * @user_data_free_func: a call back function to destroy the data object passed as @user_data
 *
 * Attach any data object to a #MIO object. Attached data can be got with
 * mio_get_user_data(). The attached data is destroyed when new data is attached to
 * the #MIO object, or #the MIO object itself is destroyed. For destroying the data
 * @user_data_free_func is used.
 *
 */

void  mio_attach_user_data (MIO *mio, void *user_data, MIODestroyNotify user_data_free_func)
{
	if (mio->udata.d && mio->udata.f)
		mio->udata.f (mio->udata.d);

	mio->udata.d = user_data;
	mio->udata.f = user_data_free_func;
}

/**
 * mio_get_user_data:
 * @mio: A #MIO object
 *
 * Returns: user data attached with mio_attach_user_data() to a #MIO object.
 *
 */
void *mio_get_user_data (MIO *mio)
{
	return mio->udata.d;
}
