Summary: Exuberant Ctags - a multi-language source code indexing tool
Name: ctags
Version: @@VERSION@@
Release: 1
License: GPL
Group: Development/Tools
Source: http://prdownloads.sourceforge.net/ctags/ctags-%{version}.tar.gz
URL: http://ctags.sourceforge.net
Buildroot: %{_tmppath}/%{name}-%{version}-root

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
%configure
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
