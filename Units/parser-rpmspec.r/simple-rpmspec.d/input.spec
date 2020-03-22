Summary: Exuberant Ctags - a multi-language source code indexing tool
Name: ctags
Version: 99
Release: 1
License: GPLv2
Group: Development/Tools
Source: http://prdownloads.sourceforge.net/ctags/ctags-%{version}.tar.gz
URL: http://ctags.sourceforge.net
Buildroot: %{_tmppath}/%{name}-%{version}-root

Patch0: empty0.patch
Patch9999: dummy1.patch

%define __scm_apply_git(qp:m:) %{__git} am

%define YES yes
%description
Exuberant Ctags generates an index (or tag) file of language objects
found in source files for many popular programming languages. This index
makes it easy for text editors and other tools to locate the indexed
items. Exuberant Ctags improves on traditional ctags because of its
multilanguage support, its ability for the user to define new languages
searched by regular expressions, and its ability to generate emacs-style
TAGS files.

Install ctags if you are going to use your system for programming.

%undef __scm_apply_git
%global perf_make make %{?_smp_mflags} -C tools/perf -s V=1 WERROR=0 NO_LIBUNWIND=1 HAVE_CPLUS_DEMANGLE=1 NO_GTK2=1 NO_STRLCPY=1 prefix=%{_prefix} lib=%{_lib}

%define install_post \
  if [ "%{with_debug}" -ne "0" ]; then \
    : \
  fi \
  if [ "%{with_default}" -ne "0" ]; then \
    : \
  fi \
%{nil}

%package docs
%description docs
Something must be written here.

%package -n universal-ctags-devel
%description universal-ctags-devel
Something must be written here.

%prep
%setup -q

%build
%configure --with-libxml=%{YES} --with-libyml=%{YES}\
	   --without-foo --enable-bar \
	   --disable-baz\
	   --with-bazz=0
make

%install
rm -rf $RPM_BUILD_ROOT
%makeinstall

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc COPYING EXTENDING.html FAQ NEWS README ctags.html
%{_bindir}/ctags
%{_mandir}/man1/ctags*
