# Taken from /usr/share/emacs/24.5/etc/emacs-buffer.gdb

set main
set $yverbose = 1
set $yfile_buffers_only = 0

define ybuffer-contents
  ydump-buffer $arg0 /dev/stdout
  if $yverbose && $buf->z_byte <= 1
    yget-current-buffer-name
    printf "[Buffer \"%s\" is empty.]\n", $ycurrent_buffer_name
  else
    if *($endptr-1) != '\n'
      echo \n
    end
  end
end
document ybuffer-contents
  Write contents of buffer N (numbered according to `ybuffer-list') to stdout.
end
