Distribution: Exuberant Ctags
Summary: A multi-language source code indexing tool
Name: ctags
Version: @@VERSION@@
Release: 1
Copyright: GPL
Group: Development/Tools
Source: http://prdownloads.sourceforge.net/ctags/ctags-%{version}.tar.gz
URL: http://ctags.sourceforge.net
Buildroot: /var/tmp/ctags-root

%description
Exuberant Ctags generates an index (or tag) file of language objects
found in source files for many popular programming languages. This index
makes it easy for text editors and other tools to locate the indexed
items. Exuberant Ctags improves on traditional ctags because of its
multilanguage support, its ability for the user to define new languages
searched by regular expressions, and its ability to generate emacs-style
TAGS files.

Install ctags if you are going to use your system for programming.

%prep
%setup -q

%build
autoconf
CFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=$RPM_BUILD_ROOT/usr --disable-etags
make CFLAGS="$RPM_OPT_FLAGS"

%install
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT/usr install-strip

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(0644,root,root,0755)
%doc COPYING EXTENDING.html FAQ NEWS README ctags.html
%attr(0755,root,root) /usr/bin/ctags
/usr/man/man1/ctags.1.gz
