# Copyright (C) 2012-2015 Dr. Alistair Ward
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

%define package		%name-%version
%define tarBall		%package.tar.gz
%define _sharedir	%prefix/share
%define _docdir		%_sharedir/doc/%name
%define _mandir		%_sharedir/man

Summary:	Finds the optimal subset of the specified files, to fit into a limited space, with minimal wastage
Name:		squeeze
Version:	1.0.4.18
Release:	1
License:	GPLv3
# From '/usr/share/doc/packages/rpm/GROUPS'.
Group:		Applications/File
Source0:	https://functionalley.com/Downloads/sdist/%tarBall
URL:		https://functionalley.com/Squeeze/%name.html
Prefix:		/usr
BuildRequires:	haskell-platform

%description
Returns progressively better subsets of the specified files, to fit into a limited space, without wasting more than the specific ratio.

%prep
# N.B.: CWD has changed to %_builddir
echo 'package="%package", prefix="%prefix", _builddir="%_builddir", buildroot="%buildroot"'
(cd $OLDPWD && cabal sdist) && tar -zxf $OLDPWD/dist/%tarBall	# Make a source-distribution & unpack it into the build-directory.
cd '%package/' && cabal configure --user --prefix='%prefix' --docdir='%_docdir'	# Tell cabal to use the user's personal package-database, to generate an appropriate "Paths" module, & where to place the documentation.

%build
cd '%package/' && cabal build	# Descend into the unpacked source-distribution and build according to the previously established configuration.

%install
cd '%package/'	# Descend into the build-directory.
cabal copy --destdir='%buildroot'	# Install the built package in the target-directory.
mkdir -p -- '%buildroot%_docdir' && mv 'changelog.markdown' 'copyright' 'README.markdown' '%buildroot%_docdir/'	# 'LICENSE' has already been copied by cabal.
mkdir -p -- '%buildroot%_mandir' && mv 'man/man1' '%buildroot%_mandir/'
rm -rf -- '%buildroot%prefix/lib/'	# The library isn't a deliverable.

%clean
rm -rf -- '%_builddir/%package/' '%buildroot/'	# Only the '.rpm' is required.

%files
%attr(0755, root, root)		%prefix/bin/%name
%attr(0644, root, root)	%doc	%_docdir/changelog.markdown
%attr(0644, root, root)	%doc	%_docdir/copyright
%attr(0644, root, root)	%doc	%_docdir/LICENSE
%attr(0644, root, root)	%doc	%_docdir/README.markdown
%attr(0644, root, root)	%doc	%_mandir/man1/%name.1.gz

%changelog
* Wed May 09 2012	Alistair Ward	<squeeze@functionalley.com>	1.0.2.4-1
First cut.

