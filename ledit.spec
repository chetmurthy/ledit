# $Id$

Name: ledit
Version: 1.10
Release: 1
Packager: Daniel de Rauglaudre <daniel.de_rauglaudre@inria.fr>
Summary: Line editor
Source0: ledit-%{version}.tar.gz
Copyright: mes couilles
Group: Development/Tools
URL: http://cristal.inria.fr/~ddr/
BuildRoot: /var/tmp/ledit

%description
The command ledit allows to edit lines one by one when running an interactive
command.

%prep
%setup

%build
make

%install
mkdir -p $RPM_BUILD_ROOT%{_prefix}/bin $RPM_BUILD_ROOT%{_prefix}/man/manl
cp ledit.out $RPM_BUILD_ROOT%{_prefix}/bin/ledit
cp ledit.l $RPM_BUILD_ROOT%{_prefix}/man/manl/ledit.l

%clean
rm -rf $RPM_BUILD_ROOT

%files
%{_prefix}/bin/ledit
%{_prefix}/man/manl/ledit.l
