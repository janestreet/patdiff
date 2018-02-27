#!/bin/bash

pkg=$1

jsrpmbuild=/j/office/app/jsrpmbuild/prod/bin/jsrpmbuild

rpm="$("${jsrpmbuild}" rpm -build-env-dir "${TMPDIR:-/tmp}" "${pkg}.pkg.sexp")"

tar -c -f "${pkg}.rpm.tar" -C "$(dirname "$rpm")" --transform="s;^;${pkg}/;" "$(basename "$rpm")"

rm "$rpm"

# Why this crazy tarball stuff?
#
# Writing this down so that future maintainers of thse rpms don't tie themselves
# into knots trying to preserve the magic tarball layout:
#
# Targets in jenga are expected to have stable file names but RPMs are expected to
# include a version number in their file names. So we wrap the RPM in a tarball.
#
# Tarballs are expected to enclose all their files in a directory (no "tarbombs").
# So we wrap the rpm in another directory.
#
# But there's no reason for linux-admin to see this wrapper directory, so we strip
# it when we extract the tarball.
