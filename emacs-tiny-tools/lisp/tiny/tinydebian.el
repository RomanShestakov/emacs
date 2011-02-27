;;; tinydebian.el --- Debian utilities.

;;{{{ Id

;; Copyright (C)    2001-2007 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinydebian-version.
;; Look at the code with folding.el

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with program. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Install

;; ........................................................ &t-install ...
;;   Put this file on your Emacs-Lisp load path, add following into your
;;   $HOME/.emacs startup file
;;
;;      (add-hook 'tinydebian-:load-hook 'tinydebian-install)
;;      (require 'tinydebian)
;;
;;   If you have any about this Emacs package:
;;
;;      M-x tinydebian-submit-bug-report    send question, feedback, bugs
;;
;;  To read the documentation after file has been loaded, call
;;
;;      M-x tinydebian-version

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Overview of features
;;
;;      This package contains utilities for the Debian System Administarator,
;;      to help administring Debian in daily tasks and submitting bug
;;      reports from Emacs. Learn more about debian at
;;      http://www.debian.org/
;;
;;      o   colorize /var/log files like messages, syslog etc.
;;      o   Report Debian bug with M-x ... #todo
;;
;;  Quick start:
;;
;;      To report bug to Debian package, like command line reportbug(1):
;;
;;          M-x tinydebian-reportbug

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: libraries

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))

(eval-and-compile
  ;;  Forward declarations to quiet byte compiler.
  (defvar gnus-newsgroup-name)
  (defvar font-lock-mode)
  (defvar font-lock-keyword-face)
  (defvar global-font-lock-mode)
  (defvar font-lock-keywords)
  (defvar font-lock-defaults)
  (autoload 'gnus-summary-article-number  "gnus-sum")
  (autoload 'gnus-summary-display-article "gnus-sum")
  (defvar gnus-article-buffer))

(ti::package-defgroup-tiny TinyDebian tinydebian-: extensions
  "Debian System administrator's grabbag of utilities.")

;;}}}
;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...

(defcustom tinydebian-:load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyDebian)

(defcustom tinydebian-:find-bug-nbr-hook '(tinydebian-bug-nbr-any)
  "*Functions to return Debian bug tracking number as string.
Default value is '(tinydebian-bug-nbr-any)."
  :type  'function
  :group 'TinyDebian)

(defcustom tinydebian-:find-email-hook '(tinydebian-email-any)
  "*Functions to return Email address as string.
Default value is '(tinydebian-email-any)."
  :type  'function
  :group 'TinyDebian)

(defcustom tinydebian-:load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyDebian)

(defcustom tinydebian-:browse-url-function
  (function tinydebian-browse-url-browse-url)
  "*Function to run for HTTP URLs. Default is `browse-url'.
To use text mode buffer inside Emacs, set value to
`tinydebian-browse-url-lynx-dump' if lynx(1) is available.

See also `browse-url-browser-function'."
  :type  'function
  :group 'TinyDebian)

;;}}}
;;{{{ setup: user config

;;; ................................................... &v-user-config ...

(defcustom tinydebian-:install-buffer-file-name-regexp
  "/debian/\\(changelog\\|.*README\\)"
  "*Activate `tinydebian-bts-mode' on buffers whose file name match regexp.
This variable is used when function `tinydebian-install' is called."
  :type  'regexp
  :group 'TinyDebian)

(defcustom tinydebian-:buffer-tiger "*Tinydebian tiger*"
  "*Buffer name where to generate tiger(1) mail report chmod fixes.
See function `tinydebian-command-audit-report-tiger'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian-:buffer-wnpp-alert "*Tinydebian wnpp-alert*"
  "*Buffer name where to generate wnpp-alert(1) report.
See function `tinydebian-command-wnpp-alert'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian-:buffer-www "*Tinydebian WWW*"
  "*Buffer name where to put WWW call results.
See `tinydebian-:browse-url-function'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian-:buffer-bug-format "*Tinydebian bug#%s*"
  "*A `format' string for buffer, where %s is substituted with bug number.
See `tinydebian-buffer-url-bug'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian-:install-gnus-newsgroup-name-regexp
  "debian"
  "*Newsgroup name regexp to match to activate `tinydebian-bts-mode'."
  :type  'string
  :group 'TinyDebian)

(defface tinydebian-:warn-face
  '((((class color) (background light))
     (:background "green"))
    (((class color) (background dark))
     (:background "sea green"))
    (((class grayscale monochrome)
      (background light))
     (:background "black"))
    (((class grayscale monochrome)
      (background dark))
     (:background "white")))
  "Face used for warnings."
  :group 'TinyDebian)

;;; Color loading section  This is messy *Blech!*
;;
(defface tinydebian-:item-face
  '((((class color) (background light))
     (:foreground "green4"))
    (((class color) (background dark))
     (:foreground "green3")))
  "Face used for noticing important items."
  :group 'TinyDebian)

(defcustom tinydebian-:font-lock-mode t
  "If non-nil, allow turning on `font-lock-mode'.")

;;}}}
;;{{{ setup: -- private

;;; ....................................................... &v-private ...

(defvar tinydebian-:font-lock-keywords-adaptive-date t
  "Flag to signal that current time is used to display today's log.
For exmple in /etc/syslog today's log log entries are highlighted
differently that previous days. However this must be changed in
next day, because the day changes.

This flags says, that adaptive-date regexps are be used.")

(make-variable-buffer-local 'tinydebian-:font-lock-keywords-adaptive-date)

(defvar tinydebian-:font-lock-keywords-bugs-rc ;; &font
  ;; Package: [59]bookmarks (optional; [60]Tobias Toedter) [bookmarks/1.4 ; =] [[61]
  ;; add/edit comment]
  ;; [62]401275 [P        N ] Remove two sites which force the user to enter a 24 mo
  ;; nth contract
  (list
   (list
    "Package: *\\[[0-9]+\\] *\\([a-z0-9.-]+\\)"
    1 'font-lock-builtin-face)
   (list
    (concat
     "^\\[[0-9]+\\][[0-9]+ *\\(\\[[^]\r\n]+\\]\\) +"
     "\\(.+"
     ;;  Continue to second line
     "\\(?:\n *[A-Za-z<>'()].*\\)?"
     "\\)")
    '(1 'font-lock-type-face)
    '(2 'font-lock-keyword-face)))
  "Font lock keywords to set after calling `tinydebian-url-list-bugs-by-rc'.
Only used if `tinydebian-:browse-url-function'is set to
`tinydebian-browse-url-lynx-dump'.")

(defvar tinydebian-:font-lock-package-bugs
  (list
   (list
    "Package: *\\[[0-9]+\\] *\\([a-z0-9.-]+\\)"
    1 'font-lock-builtin-face))
  "Font lock keywords to set after calling `tinydebian-url-list-bugs-by-rc'.
Only used if `tinydebian-:browse-url-function'is set to
`tinydebian-browse-url-lynx-dump'.")

(defconst tinydebian-:bin-dpkg (executable-find "dpkg")
  "Location of `dpkg' binary.")

(defconst tinydebian-:bin-grep-available (executable-find "grep-available")
  "Location of `grep-available' binary.")

(defvar tinydebian-:grep-find-devel-docdir-list
  '("/usr/share/doc/debian-policy"
    "/usr/share/doc/debian-reference-en"
    "/usr/share/doc/debian-reference-en"
    "/usr/share/doc/developers-reference")
  "*List of directororied to search for Debian development policy etc.")

(defvar tinydebian-:severity-list
  '(("critical"
     "Makes unrelated software on the system (or the whole system) break,
or causes serious data loss, or introduces a security hole on systems where
you install the package.")
    ("grave"
     "Makes the package in question unuseable or mostly so, or causes data
loss, or introduces a security hole allowing access to the accounts of users
who use the package.")
    ("serious"
     "Severe violation of Debian policy (that is, it violates a
\"must\" or \"required\" directive), or, in the package maintainer's
opinion, makes the package unsuitable for release.")
    ("important"
     "A bug which has a major effect on the usability of a package,
without rendering it completely unusable to everyone.")
    ("normal"
     "The default value, applicable to most bugs.")
    ("minor"
     "A problem which doesn't affect the package's usefulness, and is
presumably trivial to fix.")
    ("wishlist"
     "For any feature request, and also for any bugs that are very
difficult to fix due to major design considerations.")
    ("fixed"
     "For bugs that are fixed but should not yet be closed. This is an
exception for bugs fixed by non-maintainer uploads. Note: the "fixed"
tag should be used instead."))
  "The bug system records a severity level with each bug report.
This is set to normal by default, but can be overridden either by supplying a Severity line in the pseudo-header when the bug is submitted Severity or error.
http://www.debian.org/Bugs/Developer#severities")

(defvar tinydebian-:severity-selected nil
  "Function `tinydebian-severity-select-*' sets this to user selection.")

(defconst tinydebian-:menu-severity
  '("\
Severity: ?h)elp c)rit g)rave s)erious i)import RET-n)orm m)inor w)ish f)ixed"
    ;; NOTE: These function are automatically created, you don't find
    ;; then with C-s. See `tinydebian-install-severity-functions'
    ((?c .      ( (call-interactively 'tinydebian-severity-select-critical)))
     (?g .      ( (call-interactively 'tinydebian-severity-select-grave)))
     (?s .      ( (call-interactively 'tinydebian-severity-select-serious)))
     (?i .      ( (call-interactively 'tinydebian-severity-select-important)))
     (?n .      ( (call-interactively 'tinydebian-severity-select-normal)))
     (?\C-m .   ( (call-interactively 'tinydebian-severity-select-normal)))
     (?m .      ( (call-interactively 'tinydebian-severity-select-minor)))
     (?w .      ( (call-interactively 'tinydebian-severity-select-wishlist)))
     (?f .      ( (call-interactively 'tinydebian-severity-select-fixed)))))
  "Severity menu.

The bug system records a severity level with each bug report. This is set
to normal by default, but can be overridden either by supplying a Severity
line in the pseudo-header when the bug is submitted (see the instructions
for reporting bugs), or by using the severity command with the control
request server.

critical
    makes unrelated software on the system (or the whole system)
    break, or causes serious data loss, or introduces a security hole
    on systems where you install the package.

grave
    makes the package in question unuseable or mostly so, or causes
    data loss, or introduces a security hole allowing access to the
    accounts of users who use the package.

serious
    is a severe violation of Debian policy (that is, it violates a
    \"must\" or \"required\" directive), or, in the package
    maintainer's opinion, makes the package unsuitable for release.

important
    a bug which has a major effect on the usability of a package,
    without rendering it completely unusable to everyone.

normal
    the default value, applicable to most bugs.

minor
    a problem which doesn't affect the package's usefulness, and is
    presumably trivial to fix.

wishlist
    for any feature request, and also for any bugs that are very
    difficult to fix due to major design considerations.

fixed
    for bugs that are fixed but should not yet be closed. This is an
    exception for bugs fixed by non-maintainer uploads. Note: the
    \"fixed\" tag should be used instead. Certain severities are
    considered release-critical, meaning the bug will have an impact
    on releasing the package with the stable release of Debian.
    Currently, these are critical, grave and serious.")

(defvar tinydebian-:tags-list
  '(("already-in-ubuntu"
     "Package is in Ubuntu but not yet in Debian. This is a notice to a wishlist
See <http://utnubu.alioth.debian.org/>.xm")
    ("patch"
     "A patch or some other easy procedure for fixing the bug is included
in the bug logs. If there's a patch, but it doesn't resolve the bug
adequately or causes some other problems, this tag should not be used.")
    ("wontfix"
     "This bug won't be fixed. Possibly because this is a choice between
two arbitrary ways of doing things and the maintainer and submitter prefer
different ways of doing things, possibly because changing the behaviour
will cause other, worse, problems for others, or possibly for other reasons.")
    ("moreinfo"
     "This bug can't be addressed until more information is provided by
the submitter. The bug will be closed if the submitter doesn't provide
more information in a reasonable (few months) timeframe. This is for
bugs like "It doesn't work". What doesn't work?.")
    ("unreproducible"
     "This bug can't be reproduced on the maintainer's system.
Assistance from third parties is needed in diagnosing the cause of the problem.")
    ("help"
     "The maintainer is requesting help with dealing with this bug.")
    ("pending"
     "The problem described in the bug is being actively worked on,
i.e. a solution is pending.")
    ("fixed"
     "This bug is fixed or worked around (by a non-maintainer upload,
for example), but there's still an issue that needs to be resolved.
This tag replaces the old \"fixed\" severity.")
    ("security"
     "This bug describes a security problem in a package (e.g., bad
permissions allowing access to data that shouldn't be accessible;
buffer overruns allowing people to control a system in ways they
shouldn't be able to; denial of service attacks that should be fixed, etc).
Most security bugs should also be set at critical or grave severity.")
    ("upstream"
     "This bug applies to the upstream part of the package.")
    ("confirmed"
     "The maintainer has looked at, understands, and basically agrees
with the bug, but has yet to fix it. (Use of this tag is optional; it is
intended mostly for maintainers who need to manage large numbers of open bugs.")
    ("fixed-upstream"
     "The bug has been fixed by the upstream maintainer, but not yet
in the package (for whatever reason: perhaps it is too complicated to
backport the change or too minor to be worth bothering).")
    ("ipv6"
     "This bug affects support for Internet Protocol version 6.")
    ("lfs"
     "This bug affects support for large files (over 2 gigabytes).")
    ("l10n"
     "This bug is relevant to the localisation of the package.")
    ("woody"
     "This bug particularly applies to the (unreleased) woody distribution.")
    ("sarge"
     "This bug particularly applies to the sarge distribution.")
    ("etch"
     "This bug particularly applies to the etch distribution.")
    ("sid"
     "This bug particularly applies to an architecture that is
currently unreleased (that is, in the sid distribution).")
    ("experimental"
     "This bug particularly applies to the experimental distribution."))
  "Each bug can have zero or more of a set of given tags.
These tags are displayed in the list of bugs when you look at a
package's page, and when you look at the full bug log.
See <http://www.debian.org/Bugs/Developer#tags>.")

(defvar tinydebian-:wnpp-buffer "*TinyDebian WNPP*"
  "WNPP question buffer.")

(defvar tinydebian-:menu-wnpp-selected nil
  "Placeholder of selection from `tinydebian-:menu-wnpp'.")

(defconst tinydebian-:menu-wnpp
  (list
   '(format
     "TinyDebian:WNPP%s 1i)tp 2o)rphan 3a)dopt 4n)ew package ?)help q)uit"
     (if tinydebian-:menu-wnpp-selected
         (format ";%s " (symbol-name tinydebian-:menu-wnpp-selected))
       ""))
   (list
    '(?1 . ( (setq tinydebian-:menu-wnpp-selected 'package)))
    '(?i . ( (setq tinydebian-:menu-wnpp-selected 'package)))
    '(?I . ( (setq tinydebian-:menu-wnpp-selected 'package)))
    '(?p . ( (setq tinydebian-:menu-wnpp-selected 'package)))
    '(?P . ( (setq tinydebian-:menu-wnpp-selected 'package)))
    '(?2 . ( (setq tinydebian-:menu-wnpp-selected 'oprhan)))
    '(?o . ( (setq tinydebian-:menu-wnpp-selected 'oprhan)))
    '(?O . ( (setq tinydebian-:menu-wnpp-selected 'oprhan)))
    '(?3 . ( (setq tinydebian-:menu-wnpp-selected 'adopt)))
    '(?a . ( (setq tinydebian-:menu-wnpp-selected 'adopt)))
    '(?A . ( (setq tinydebian-:menu-wnpp-selected 'adopt)))
    '(?4 . ( (setq tinydebian-:menu-wnpp-selected 'new)))
    '(?n . ( (setq tinydebian-:menu-wnpp-selected 'new)))
    '(?N . ( (setq tinydebian-:menu-wnpp-selected 'new)))))
  ;;  This message is straight from reportbug(1)
  ;;  'apt-get install reportbug'
  "What request type? If none of these things mean anything to you, or
you are trying to report a bug in an existing package)

1 p    ITP, `Intent To Package'. Please submit a package description
       along with copyright and URL in such a report.

2 o    The package has been `Orphaned'. It needs a new maintainer as soon as
       possible.

3 a    RFA, this is a `Request for Adoption'. Due to lack of time, resources,
       interest or something similar, the current maintainer is asking for
       someone else to maintain this package. He/she will maintain it in the
       meantime, but perhaps not in the best possible way. In short: the
       package needs a new maintainer.

4 n    RFP, this is a `Request For Package'. You have found an interesting piece of
       software and would like someone else to maintain it for Debian. Please
       submit a package description along with copyright and URL in such a
       report.

q      Quit menu.
")

(defconst tinydebian-:rfp-template "\
Package: wnpp
Severity: wishlist

* Package name    : <package>
  Version         : x.y.z
  Upstream Author : Name <somebody@example.org>
* URL             : <homepage: http://www.example.org/>
* License         : <license: GPL, LGPL, BSD, MIT/X, etc.>
  Programming Lang: <C, C++, C#, Perl, Python, etc.>
  Description     : <short desc>

\(Include the long description here.)
"
  "Wnpp RFP/ITP template.
NOTE: The <TAG:> constructs must be retained.")

(defvar tinydebian-:rfp-hook nil
  "Hook run after function `tinydebian-bts-mail-type-rfp'.
See also `tinydebian-:rfp-template'")

(defconst tinydebian-:wnpp-template-licenses-alist
  '("Artistic"
    "BSD"
    "GPL"
    "GPL-2"
    "LGPL"
    "LGPL-2"
    "MIT/X11")
  "List of licenses as recorded in Debian /usr/share/common-licenses/
See also <http://www.debian.org/legal/licenses/> and
<http://people.debian.org/~bap/dfsg-faq.html>.")

(defconst tinydebian-:rfs-template "\

I'm looking for sponsor:

  Package name    : <package>
  Version         : x.y.z
  ITA/ITP URL     : <ita: http://bugs.debian.org/BugNbr>
* Package bugs URL: <bugs: http://bugs.debian.org/Package>
  URL             : <mentors: http://mentors.debian.net/debian/pool/main/p/package/*.dsc>
  License         : <license: GPL, LGPL, BSD, MIT/X, Artistic, etc.>
  Programming Lang: <C, C++, C#, Perl, Python, etc.>

\(* = remove if package is not in Debian.)
Description:

debian/changelog:

Other notes:
"
  "RFS message to debian.devel.mentor mailinf list.
NOTE: The <TAG:> constructs must be retained.
See also `tinydebian-:rfs-hook'.")

(defvar tinydebian-:rfs-hook nil
  "Hook run after function `tinydebian-bts-mail-type-rfs'.
See also `tinydebian-:rfs-template'")

(defvar tinydebian-:bts-email-address "bugs.debian.org"
  "Email address or Debian Bug Tracking System.")

;; https://help.launchpad.net/UsingMaloneEmail
(defvar tinydebian-:launchpad-email-address "bugs.launchpad.net"
  "Email address or Debian Bug Tracking System.")

(defvar tinydebian-:list-email-address "lists.debian.org"
  "Email address or Debian mailing lists.")

(defvar tinydebian-:url-http-package-search
  ;; http://packages.debian.net/search?keywords=chbg&searchon=names
  "http://packages.debian.net/search?"
  "The packages Debian control URL without parameter, up to '?' token.")

(defconst tinydebian-:url-http-package-bugs
  "http://bugs.debian.org"
  "The bugs Debian control URL without parameter, up to '/' token.")

(defvar tinydebian-:url-http-debian-www
  "http://www.debian.org"
  "The main WWW page of Debian.")

(defvar tinydebian-:url-http-wnpp-page-main
  "http://www.debian.org/devel/wnpp"
  "The WNPP main page URL address. No trailing slash.")

(defconst tinydebian-:url-http-wnpp-page-alist
  '(("RFA" . "rfa_bypackage")
    ("O"   . "orphaned")
    ("RFH" . "help_request")
    ("RFP" . "requested")
    ("ITP" . "being_packaged"))
  "List of mapping to pages under `tinydebian-:url-http-wnpp-page-main'.")

(defconst tinydebian-:url-debian-page-alist
  (list
   '(bts-control
     "http://www.debian.org/Bugs/server-control")
   ;; 2006-11-06 unofficial
   (list 'bugs-rc
         "http://bts.turmzimmer.net/details.php"
         tinydebian-:font-lock-keywords-bugs-rc)
   '(qa-developer-status
     "http://qa.debian.org/developer.php?")
   '(qa-developer-bugs
     "http://bugs.debian.org/cgi-bin/pkgreport.cgi?")
   '(dfsg-license-faq
     "http://people.debian.org/~bap/dfsg-faq.html")
   '(base-files-faq
     "http://ftp.debian.org/doc/base-files/FAQ")
   '(debcheck-package
     "http://qa.debian.org/debcheck.php?dist=%s&package=%s")
   '(mentors
     "http://mentors.debian.net")
   '(mentors-pkg-pool
     "http://mentors.debian.net/debian/pool")
   '(pkg-search-files
     "http://packages.debian.org/cgi-bin/search_contents.pl?searchmode=searchfiles&case=insensitive")
   '(developers-reference
     "http://www.debian.org/doc/packaging-manuals/developers-reference/")
   ;;  apt-get install debian-reference-common debian-reference-en
   '(developers-reference-text
     "/usr/share/doc/Debian/reference/reference.en.txt.gz")
   '(policy
     "http://www.debian.org/doc/debian-policy/index.html")
   '(policy-text
     "/usr/share/doc/debian-policy/policy.txt.gz")
   '(newmaint-guide
     "http://www.debian.org/doc/maint-guide/")
   '(best-practises
     "http://www.debian.org/doc/packaging-manuals/developers-reference/ch-best-pkging-practices.en.html"))
  "List of Debian site pages.
Format:
 '((PAGE-TYPE  URL [FONT-LOCK-KEYWORDS])
   ...)

The FONT-LOCK-KEYWORDS is only used if the results appear in `tinydebian-:buffer-www'.
See `tinydebian-:browse-url-function'.")

;;}}}
;;{{{ setup: -- version

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinydebian-version "tinydebian" "Display commentary." t)
(eval-and-compile
  (ti::macrof-version-bug-report
   "tinydebian.el"
   "tinydebian"
   tinydebian-:version-id
   "$Id: tinydebian.el,v 1.97 2007/08/04 10:09:46 jaalto Exp $"
   '(tinydebian-version-id
     tinydebian-:load-hook
     tinydebian-:font-lock-keywords-adaptive-date
     tinydebian-:bin-dpkg
     tinydebian-:severity-list
     tinydebian-:severity-selected
     tinydebian-:tags-list)))

(defvar tinydebian-:bts-extra-headers
  (format "X-Bug-User-Agent: Emacs %s and tinydebian.el %s\n"
          emacs-version
          (substring tinydebian-:version-id 21 25))
  "Header to add to BTS control mails.")

;;}}}
;;{{{ Install: bindings

;;; ........................................................ &bindings ...

;; #todo:
(defun tinydebian-default-bindings ()
  "Define default key bindings to `tinydebian-mode-map'.")

(eval-and-compile

;;;###autoload (autoload 'tinydebian-bts-mode          "tinydebian" "" t)
;;;###autoload (autoload 'turn-on-tinydebian-bts-mode  "tinydebian" "" t)
;;;###autoload (autoload 'turn-off-tinydebian-bts-mode "tinydebian" "" t)
;;;###autoload (defvar tinydebian-:bts-mode-prefix-key "\C-c-")
  (ti::macrof-minor-mode-wizard
   "tinydebian-bts-" " Tdeb" "\C-c-" "Tdeb" 'TinyDebian "tinydebian-:bts-" ;1-6

   "Debian Bug Tracking System (BTS) Minor mode. With this mode you can
jump to a bug report at or near current point (using browser), send
control messages, like turning RFS into ITP, send new RFS, send new
ITP etc.

Prefix key is:

  tinydebian-:bts-mode-prefix-key

Mode description:

\\{tinydebian-:bts-mode-prefix-map}"

   "TinyDebian BTS"
   nil
   "TinyDebian BTS minor mode menu."
   (list
    tinydebian-:bts-mode-easymenu-name
    ["Reply to bug"                  tinydebian-bts-mail-type-reply            t]
    ["Report bug by mail"            tinydebian-bug-report-mail                t]
    ["Goto URL by bug number"        tinydebian-bug-browse-url-by-bug          t]
    ["Goto URL by package bugs"      tinydebian-bug-browse-url-by-package-bugs t]
    ["Goto URL by package name"      tinydebian-bug-browse-url-by-package-name t]

    "----"

    (list
     "BTS WNPP messages"
     ["Send BTS ITA: intent to adopt"      tinydebian-bts-mail-type-ita    t]
     ["Send BTS ITP: reponse to RFP"       tinydebian-bts-mail-type-itp    t]
     ["Send BTS RFA: request for adopt"    tinydebian-bts-mail-type-rfa    t]
     ["Send BTS RFH: request for help"     tinydebian-bts-mail-type-rfh    t]
     ["Send BTS RFP: request for packege"  tinydebian-bts-mail-type-rfp    t]
     ["Send BTS RFS: request for sponsor"  tinydebian-bts-mail-type-rfs    t]
     ["Send BTS O: orphan"                 tinydebian-bts-mail-type-orphan t]
     ["WNPP control menu"                  tinydebian-package-wnpp-main    t])

    (list
     "BTS Control messages"
     ["Send BTS Ctrl close"                tinydebian-bts-mail-ctrl-close    t]
     ["Send BTS Ctrl severity"             tinydebian-bts-mail-ctrl-severity t]
     ["Send BTS Ctrl tags"                 tinydebian-bts-mail-ctrl-tags     t]
     ["Send BTS Ctrl usertag"              tinydebian-bts-mail-ctrl-usertag  t]
     ["Send BTS Ctrl forward"              tinydebian-bts-mail-ctrl-forward-main  t]
     ["Send BTS Ctrl reassign"             tinydebian-bts-mail-ctrl-reassign t]
     ["Send BTS Ctrl retitle"              tinydebian-bts-mail-ctrl-retitle  t]
     ["Send BTS Ctrl reopen"               tinydebian-bts-mail-ctrl-reopen   t])

    (list
     "Query information"
     ["List of WNPP RFP"           tinydebian-url-list-wnpp-rfp            t]
     ["List of WNPP RFH"           tinydebian-url-list-wnpp-rfh            t]
     ["List of WNPP RFA"           tinydebian-url-list-wnpp-rfa            t]
     ["List of WNPP Orphaned"      tinydebian-url-list-wnpp-orphaned       t]
     ["List of WNPP ITP"           tinydebian-url-list-wnpp-itp            t]
     ["List of RC bugs"            tinydebian-url-list-bugs-by-rc          t]
     ["List of items by usertag"   tinydebian-url-list-bugs-by-usertag     t]
     ["Installed pkg problems"     tinydebian-command-show-wnpp-alert      t]
     ["Grep devel documentation"   tinydebian-grep-find-debian-devel       t]

     "----"

     ["QA Developer status"        tinydebian-url-list-qa-developer-status t]
     ["QA Developer bugs"          tinydebian-url-list-qa-developer-bugs   t]
     ["Package debcheck"           tinydebian-url-list-package-debcheck    t]
     ["Package search by name"     tinydebian-url-list-package-by-package-name t]
     ["Package search by filename" tinydebian-url-list-package-by-filename t]

     "----"

     ["FAQ DFSG and licenses"      tinydebian-url-list-dsfg-license-faq    t]
     ["FAQ base files"             tinydebian-url-list-base-files-faq      t])

    (list
     "Debian manuals"
     ["URL BTS Ctrl page"            tinydebian-url-bts-ctrl-page         t]
     ["URL Policy manual"            tinydebian-url-policy-manual         t]
     ["URL Newmaint guide"           tinydebian-url-policy-new-maintainer-guide  t]
     ["URL Developer's reference"    tinydebian-url-policy-developers-reference  t]
     ["URL Best practises"           tinydebian-url-policy-best-practises t]))

   (progn

     (define-key map  "b"  'tinydebian-bug-browse-url-by-bug)
     (define-key map  "B"  'tinydebian-bug-browse-url-by-package-bugs)
     (define-key map  "M"  'tinydebian-bug-report-mail)
     (define-key map  "p"  'tinydebian-bug-browse-url-by-package-name)
     (define-key map  "r"  'tinydebian-bug-reply)
     (define-key map  "w"  'tinydebian-package-wnpp-main)

     (define-key map  "-a" 'tinydebian-bts-mail-type-ita)
     (define-key map  "-A" 'tinydebian-bts-mail-type-rfa)
     (define-key map  "-h" 'tinydebian-bts-mail-type-rfh)
     (define-key map  "-P" 'tinydebian-bts-mail-type-itp)
     (define-key map  "-p" 'tinydebian-bts-mail-type-rfp)
     (define-key map  "-r" 'tinydebian-bts-mail-type-reply)
     (define-key map  "-s" 'tinydebian-bts-mail-type-rfs)
     (define-key map  "-o" 'tinydebian-bts-mail-type-orphan)
     (define-key map  "mi" 'tinydebian-bts-mail-message-info)

     ;;  (i)nfo (i)nstalled
     (define-key map  "ii" 'tinydebian-command-show-wnpp-alert)

     ;;  (i)nfo (g)rep
     (define-key map  "ig" 'tinydebian-grep-find-debian-devel)

     ;;  (L)ist Url commands
     ;; (b)ugs
     (define-key map  "lbr"  'tinydebian-url-list-bugs-by-rc)
     (define-key map  "lbu"  'tinydebian-url-list-bugs-by-usertag)
     ;; (d)eveloper
     (define-key map  "ldb"  'tinydebian-url-list-qa-developer-bugs)
     (define-key map  "lds"  'tinydebian-url-list-qa-developer-status)
     ;; (f)aq
     (define-key map  "lfl"  'tinydebian-url-list-dsfg-license-faq)
     (define-key map  "lfb"  'tinydebian-url-list-base-files-faq)
     ;; (p)ackage
     (define-key map  "lpf"  'tinydebian-url-list-package-by-filename)
     (define-key map  "lpp"  'tinydebian-url-list-package-by-package-name)
     (define-key map  "lpc"  'tinydebian-url-list-package-debcheck)
     ;; (w)npp
     (define-key map  "lwa"  'tinydebian-url-list-wnpp-rfa)
     (define-key map  "lwh"  'tinydebian-url-list-wnpp-rfh)
     (define-key map  "lwo"  'tinydebian-url-list-wnpp-orphaned)
     (define-key map  "lwp"  'tinydebian-url-list-wnpp-rfp)
     (define-key map  "lwP"  'tinydebian-url-list-wnpp-itp)

     ;;  (C)ontrol commands
     (define-key map  "cc"  'tinydebian-bts-mail-ctrl-close)
     (define-key map  "cs"  'tinydebian-bts-mail-ctrl-severity)
     (define-key map  "ct"  'tinydebian-bts-mail-ctrl-tags)
     (define-key map  "cT"  'tinydebian-bts-mail-ctrl-usertag)
     (define-key map  "cf"  'tinydebian-bts-mail-ctrl-forward-main)
     (define-key map  "cr"  'tinydebian-bts-mail-ctrl-reassign)
     (define-key map  "cR"  'tinydebian-bts-mail-ctrl-retitle)
     (define-key map  "co"  'tinydebian-bts-mail-ctrl-reopen)

     ;;  URLs
     (define-key map  "ub"  'tinydebian-url-bts-ctrl-page)
     (define-key map  "ud"  'tinydebian-url-policy-developers-reference)
     (define-key map  "un"  'tinydebian-url-policy-new-maintainer-guide)
     (define-key map  "up"  'tinydebian-url-policy-manual)
     (define-key map  "uP"  'tinydebian-url-policy-best-practises))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mode-gnus-summary-maybe-turn-on ()
  "Activate tinydebian-bts-mode if group name contains word 'Debian'"
  (when (and (boundp 'gnus-newsgroup-name)
             (stringp gnus-newsgroup-name)
             (string-match "debian" gnus-newsgroup-name))
    (turn-on-tinydebian-bts-mode)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mode-maybe-turn-on ()
  "Activate tinydebian-bts-mode if buffer contains word 'Debian'"
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "debian" nil t))
    (turn-on-tinydebian-bts-mode)))

;;}}}
;;{{{ Install: generate severity function etc.

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-install-severity-functions ()
  "Generate `tinydebian-severity-select-*' user functions."
  ;; Generate functions on run-time.
  (mapcar
   (function
    (lambda (x)
      (let ((sym (intern (format "tinydebian-severity-select-%s"  x)))
            def)
        (setq def
              (` (defun (, sym) ()
                   "Set Severity level `tinydebian-:severity-selected'."
                   (interactive)
                   (setq  tinydebian-:severity-selected (, x)))))
        (eval def))))
   '("critical"
     "grave"
     "serious"
     "important"
     "normal"
     "minor"
     "wishlist"
     "fixed")))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-find-file-hooks ()
  "Run `tinydebian-bts-mode-maybe-turn-on'.
Install `font-lock-keywords' for log files."
  (tinydebian-bts-mode-maybe-turn-on)
  (tinydebian-font-lock-keywords))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-install-font-lock-keywords (&optional uninstall)
  "Install colors to all current buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (tinydebian-font-lock-keywords uninstall))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydebian-install-in-buffers (&optional uninstall)
  "Install or UNINSTALL `tinydebiab-bts-mode' in existing buffers.
Activate on Gnus summary and article modes if there is word 'Debian'.
Activate on files whose path matches
`tinydebian-:install-buffer-file-name-regexp'."
  (flet ((search (regexp)
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward regexp nil t))))
    (dolist (buffer (buffer-list))
      (let (doit)
        (with-current-buffer buffer
          (cond
           ((and (stringp buffer-file-name)
                 (string-match tinydebian-:install-buffer-file-name-regexp
                               buffer-file-name))
            (setq doit t))
           ((and (eq major-mode 'gnus-summary-mode)
                 (boundp 'gnus-newsgroup-name)
                 (string-match
                  tinydebian-:install-gnus-newsgroup-name-regexp
                  gnus-newsgroup-name))
            (setq doit t))
           ((and (eq major-mode 'gnus-article-mode)
                 (search "debian"))
            (setq doit t))
           ((search (concat
                     "bug#[0-9][0-9][0-9][0-9][0-9][0-9]\\>"
                     "\\|Closes +#[0-9][0-9][0-9][0-9][0-9][0-9]"))
            (setq doit t)))
          (if uninstall
              (turn-off-tinydebian-bts-mode)
            (turn-on-tinydebian-bts-mode)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-install (&optional uninstall)
  "Install or UNINSTALL package."
  (interactive "P")
  ;;  This just hides from byte compiler function definition
  ;;  so that it does not remember how amny arguments it takes
  ;;
  ;;  function tinydebian-bug-report-mail used to take 0+ arguments,
  ;;  now takes 1 function tinydebian-bug-report-mail defined multiple
  ;;  times in this file
  ;;
  (cond
   (uninstall
    ;;(remove-hook 'write-file-hooks 'tinydebian-auto-save)
    (tinydebian-install-font-lock-keywords 'uninstall)
    (remove-hook 'find-file-hooks 'tinydebian-find-file-hooks)
    (remove-hook 'gnus-summary-prepare-hook
                 'tinydebian-bts-mode-gnus-summary-maybe-turn-on)
    (remove-hook 'gnus-article-prepare-hook
                 'tinydebian-bts-mode-maybe-turn-on)
    (tinydebian-install-in-buffers 'uninstall))
   (t
    ;; (add-hook 'write-file-hooks 'tinydebian-auto-save)
    (tinydebian-install-font-lock-keywords)
    (add-hook 'find-file-hooks  'tinydebian-find-file-hooks)
    (add-hook 'gnus-summary-prepare-hook
              'tinydebian-bts-mode-gnus-summary-maybe-turn-on)
    (add-hook 'gnus-article-prepare-hook
              'tinydebian-bts-mode-maybe-turn-on)
    (tinydebian-install-in-buffers)))
  nil)

;;}}}
;;{{{ Utility functions

;;; ----------------------------------------------------------------------
;;;
(defmacro tinydebian-launchpad-email-compose (address)
  "Send message to Launchpad at ADDRESS."
  `(format "%s@%s" ,address tinydebian-:launchpad-email-address))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-launchpad-email-new ()
  (tinydebian-launchpad-email-compose "new"))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinydebian-list-email-compose (address)
  "Send message to Debian mailing list at ADDRESS."
  `(format "%s@%s" ,address tinydebian-:list-email-address))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinydebian-bts-email-compose (address)
  "Send message to Debian BTS at ADDRESS."
  `(format "%s@%s" ,address tinydebian-:bts-email-address))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-email-submit ()
  (tinydebian-bts-email-compose "submit"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-email-control ()
  (tinydebian-bts-email-compose "control"))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinydebian-package-narrow-to-region (&rest body)
  "Search dpkg -s result from current point forward and narrow around it.
Point is put at the beginning of region.
Variable `package' contains the package name."
  (`
   (let* (beg-narrow
          package)
     (when (re-search-forward "^Package: +\\([^ \t\r\n]+\\) *$" nil t)
       (setq beg-narrow (line-beginning-position))
       (setq package (match-string 1))
       (when (re-search-forward "^[ \t]*$" nil t)
         (ti::narrow-safe beg-narrow (point)
           (ti::pmin)
           (,@ body)))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-with-buffer-macro 'edebug-form-spec '(body))
(put 'tinydebian-with-buffer-macro 'lisp-indent-function 0)
(defmacro tinydebian-with-buffer-macro (buffer &rest body)
  "Create BUFFER, empty it and run BODY.
Variable `buffer' is available in this macro."
  `(let ((buffer (get-buffer-create ,buffer)))
     (with-current-buffer buffer
       (erase-buffer)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-string-p (str &optional error)
  "Check that STR contains non-empty value.
Signal optional ERROR message is STR was empty."
  (or (and (stringp str)
           (string-match "[^ \t\r\n]" str))
      (and (stringp error)
           (error "TinyDebian: %s" error))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-buffer-match-string (regexp &optional start)
  "Search REGEX at optional START point and return submatch 1."
  (save-excursion
    (if start
        (goto-char start))
    (if (re-search-forward regexp nil t)
        (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-call-process (prg &optional buffer &rest args)
  "Call PRG with list of ARGS and print output to current buffer or BUFFER."
  (apply 'call-process
         prg
         (not 'infile)
         (or buffer (current-buffer))
         (not 'real-time-display)
         args))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-packages-browse-url-compose
  (keyword &optional search-on distribution section)
  "Return URL search string.
Argument: KEYWORD
Optional: SEARCH-ON DISTRIBUTION SECTION."
  (format (concat tinydebian-:url-http-package-search
                  "keywords=%s&"
                  "searchon=%s&"
                  "subword=1&"
                  "version=%s&"
                  "release=%s")
          keyword
          (or search-on    "names")
          (or distribution "all")
          (or section      "all")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-string-delete-newlines (string)
  "Delete newlines from STRING."
  (ti::string-regexp-delete "[\r\n]" string))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-read-license (message)
  "Ask license with MESSAGE.
  See `tinydebian-:wnpp-template-licenses-alist'."
  (completing-read
   message
   (mapcar (lambda (x)
             (cons x 1))
           tinydebian-:wnpp-template-licenses-alist)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-font-lock-keywords (&optional uninstall)
  "Add color support to various log files by setting
`font-lock-keywords'."
  (interactive)
  (let* ((today  (ti::date-standard-rfc-regexp "mon-date"))
         ;; (cs     (or comment-start-skip "[ \t]+"))
         (file   "")
         keywords)
    (when (stringp buffer-file-name)
      (setq file (or buffer-file-name "no-name?")))
    (setq
     keywords
     (cond
      ;; ............................................. Linux log files ...
      ;; /var/log/
      ((string-match "/log/messages$" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
                    0 'font-lock-function-name-face)
              (list
               (concat
                "^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              (list
               (concat "restarted\\|started"
                       "\\|ignoring"
                       "\\|Linux version.*")
               0 'font-lock-comment-face))))

      ((string-match "mail\\.log\\|mail\\.info" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
                    0 'font-lock-function-name-face)
              (list
               (concat
                "^... +[0-9]+ ++[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              '("timed out\\|did not.*"
                0 tinydebian-:warn-face)
              (list
               (concat "\\(from\\|to\\)=\\([^ ,\t\r\n]+\\)")
               2 'font-lock-comment-face))))

      ((string-match "daemon\\.log" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list
               (concat
                "^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              (list
               (concat "connection attempt" ;);  See "iplogger" package
                       0 'tinydebian-:warn-face)
               (list
                (concat "signal +[0-9]+\\|no such user"
                        "\\|connect from .*")
                0 'font-lock-comment-face)))))

      ((string-match "auth\\.log" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
                    0 'font-lock-function-name-face)
              (list
               (concat
                "^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              (list
               (concat "opened +for +[^ \t\r\n]+")
               0 'tinydebian-:warn-face)
              '( "for user \\(root\\)"
                 1 font-lock-string-face)
              '( "from \\([^ \t\r\n]+\\)"
                 1 font-lock-type-face)
              '( "for +\\([^ \t\r\n]+\\) +from"
                 1 font-lock-comment-face)
              '( "for user +\\([^ \t\r\n]+\\)"
                 1 font-lock-comment-face))))

      ((string-match "syslog" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
                    0 'font-lock-function-name-face)
              (list
               (concat
                "^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              (list
               (concat "Invalid.*"
                       ;; portmap[135]: cannot bind udp: Address already in use
                       "\\|cannot"
                       "\\|Connection timed out"
                       ;;  See iplogger(1)
                       "\\|connection attempt"
                       ;;  See portsentry(1)
                       "\\|attackalert:.* +to +.*port.*"
                       ;;  apm -s failed
                       "\\| failed"
                       "\\|did not .*")
               0 'tinydebian-:warn-face)
              '("to=\\([^ \t\r\n]+\\)"
                1 font-lock-comment-face)
              '("(\\([^ )\t\r\n]+\\)) CMD "
                1 font-lock-comment-face)
              '("CMD .*"
                0 font-lock-constant-face)
              '("inetd"2
                0 font-lock-type-face)
              (list
               (concat
                "program exit.*\\|.*started.*"
                ;;  btpd daemon
                "\\|synchronisation lost")
               0 font-lock-keyword-face))))))
    (when keywords
      (cond
       (uninstall
        (setq font-lock-keywords nil))
       ((or font-lock-mode
            tinydebian-:font-lock-mode
            global-font-lock-mode
            (font-lock-mode-maybe 1))
        (setq font-lock-keywords keywords))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-at-word (&optional string)
  "Read email address if any at current point or from STRING."
  (or string
      (setq string (thing-at-point 'url)))
  (when (and (stringp string)
             (string-match "mailto:\\(.+\\)" string))
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-at-line (&optional string)
  "Read email address if any at current line or from STRING."
  (or string
      (setq string (thing-at-point 'line)))
  (when (and (stringp string)
             (string-match "[^ <\t\r\n]+@[^ \t\r\n>]+" string))
    (match-string 0 string)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-email-gnus-summary-mode-macro 'edebug-form-spec '(body))
(put 'tinydebian-email-gnus-summary-mode-macro 'lisp-indent-function 0)
(defmacro tinydebian-email-gnus-summary-mode-macro (&rest body)
  "At current poiint, examine article and run BODY."
  `(when (eq major-mode 'gnus-summary-mode)
     (let ((article (gnus-summary-article-number))
           article-window)
       (gnus-summary-display-article article)
       (setq article-window (get-buffer-window gnus-article-buffer t))
       (gnus-eval-in-buffer-window gnus-article-buffer
                                   ,@body))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-gnus-summary-mode ()
  "Read mail address if point is at Gnus summary buffer."
  (tinydebian-email-gnus-summary-mode-macro
   (tinydebian-email-field-from)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-field-from ()
  "Read From: field and return email."
  (let* ((str (mail-fetch-field "From")))
    (or (and str
             (tinydebian-email-at-line str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-field-to ()
  "Read To: field and return email."
  (let* ((str (mail-fetch-field "To")))
    (or (and str
             (tinydebian-email-at-line str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-any (&rest args)
  "Try various methods to find email address. Ignore ARGS.
At current point, current line, headers of the mail message."
  (or (tinydebian-email-gnus-summary-mode)
      (tinydebian-email-at-word)
      (tinydebian-email-at-line)
      (tinydebian-email-field-from)
      (tinydebian-email-field-to)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-email-search ()
  "Call hook `tinydebian-:find-email-hook' until value returned."
  (run-hook-with-args-until-success 'tinydebian-:find-email-hook))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-string-parse-wnpp-alert (str)
  "Parse wnpp-alert(1) like line. Return '(bug package bug-type desc)
  RFA 321654 debtags -- Enables support for package tags."
  (let (case-fold-search)
    (when (string-match
           (concat
            "\\<\\(RF.\\|IT.\\|O\\) +\\([0-9]+\\) +"
            "\\([^ \t\r\n]+\\) +-- +\\(.+[^ \t\r\n]\\)")
           str)
      (list
       (match-string 2 str)
       (match-string 3 str)
       (match-string 1 str)
       (match-string 4 str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-nbr-string (str)
  "Read bug nbr from STR."
  (or (and (string-match "#\\([0-9]+\\)" str)
           (match-string 1 str))
      (multiple-value-bind (bug)
          (tinydebian-bug-string-parse-wnpp-alert str)
        bug)
      ;;   NNNN@bugs.debian.org
      (and (string-match (concat "\\([0-9]+\\)\\(?:-[a-z]+\\)?@"
                                 tinydebian-:bts-email-address)
                         str)
           (match-string 1 str))
      ;;   BTS message lines: "owner NNNNNN"
      (and (string-match (concat "\\<\\(?:owner\\|retitle\\) "
                                 "\\([0-9][0-9][0-9][0-9][0-9][0-9]\\)\\>")
                         str)
           (match-string 1 str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-nbr-at-current-point ()
  "Read bug number with hash (#) mark from current point"
  (let ((table (syntax-table))
        word)
    (with-syntax-table table
      (modify-syntax-entry ?# "w" table)
      (tinydebian-bug-nbr-string (current-word)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-any-at-current-point ()
  "Read bug number NNNNNN from current point"
  (let ((str (current-word)))
    (if (string-match
         "\\([^0-9]\\|^\\)\\([0-9][0-9][0-9][0-9][0-9][0-9]\\)$"
         str)
        (match-string 2 str))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-current-line ()
  "Read bug number from current line"
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (tinydebian-bug-nbr-string line)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-forward (&optional regexp)
  "Read bug#NNNN from current point forward.
If optional REGEXP is sebt, it must take number in submatch 1."
  (tinydebian-buffer-match-string (or regexp "Bug#\\([0-9]+\\)")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-hash-forward ()
  "Search #NNNN forward."
  (tinydebian-bug-nbr-forward "#\\([0-9]+\\)"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-buffer (&optional regexp)
  "Read bug#NNNN or REGEXP from buffer."
  (save-excursion
    (goto-char (point-min))
    (tinydebian-bug-nbr-forward)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-hash-buffer ()
  "Search #NNNN from buffer."
  (tinydebian-bug-nbr-buffer "#\\([0-9]+\\)"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-email-cc-to-bug-nbr ()
  "Read BTS number from CC or To"
  (let* ((str (mail-fetch-field "To")))
    (or (and str
             (tinydebian-bug-nbr-string str))
        (and (setq str (mail-fetch-field "Cc"))
             (tinydebian-bug-nbr-string str)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-email-subject-bug-nbr ()
  "Read BTS number from Subject"
  (let* ((subject (mail-fetch-field "Subject")))
    (and subject
         (tinydebian-bug-nbr-string subject))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-nbr-any (&rest args)
  "Try various methods to find bug tracking number. Ignore ARGS.
At current point, current line, headers of the mail message
(CC, To, Subject), forward from point, whole buffer."
  (or (tinydebian-bug-nbr-at-current-point)
      (tinydebian-bug-nbr-current-line)
      (tinydebian-email-cc-to-bug-nbr)
      (tinydebian-email-subject-bug-nbr)
      (tinydebian-bug-nbr-forward)
      (tinydebian-bug-nbr-buffer)
      (tinydebian-bug-hash-forward)
      (tinydebian-bug-hash-buffer)
      (tinydebian-bug-nbr-any-at-current-point)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-search ()
  "Call hook `tinydebian-:find-bug-nbr-hook' until value returned."
  (run-hook-with-args-until-success 'tinydebian-:find-bug-nbr-hook))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-package-name-header-pool ()
  "Search Filename: pool/main/p/<package>."
  (tinydebian-buffer-match-string
   "^Filename: pool.*/\\([^/ \t\r\n]+\\)/"
   (point-min)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-package-name-header-package ()
  "Search Package: <package>."
  (tinydebian-buffer-match-string
   "^Package: +\\([^/ \t\r\n]+\\)/"
   (point-min)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-parse-string-with-bug (str)
  "Return '(bug type package description) for common matches."
  (let (bug
        type
        package
        desc
        case-fold-search)
    (cond
     ((string-match "\\<\\([A-Z][A-Z][A-Z]\\|O\\): *\\(.*\\)" str)
      (setq type (match-string 1 str)
            desc (match-string 2 str)
            bug  (tinydebian-bug-nbr-string str))
      (when (string-match "^\\([a-z].+\\) +--+ *\\(.*\\)" desc)
        (setq package (match-string 1 desc)
              desc    (match-string 2 desc))))
     ((string-match "Bug#\\([0-9]+\\): *\\(.*\\)" str)
      (setq bug  (match-string 1 str)
            desc (match-string 2 str))))
    (list bug type package desc)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-parse-string-with-package (str)
  "Return '(package description) for common matches."
  (let (case-fold-search)
    (cond
     ((string-match
       "[fF]ixed in\\(?: NMU of\\)? \\([a-z][^ \t\r\n]+\\) +\\(.*\\)" str)
      (list (match-string 1 str)
            str))
     ((string-match "^\\([a-z][a-z0-9-]+\\): +\\(.*\\)" str)
      (list (match-string 1 str)
            (match-string 2 str))))))

;;; ----------------------------------------------------------------------
;;; (tinydebian-bts-parse-string-1 "Bug#353353: RFP: appweb -- very ...")
;;; (tinydebian-bts-parse-string-1 "Bug#352429: marked as done (ITA: cdrdao  -- records CDs in Disk-At-Once (DAO) mode)")
;;; (tinydebian-bts-parse-string-1 "Bug#351502: fixed in nvu 1.0final-1")
;;; (tinydebian-bts-parse-string-1 "Bug#352533: Fixed in NMU of sa-exim 4.2-3")
;;; (tinydebian-bts-parse-string-1 "Bug#244582: UFO:AI is back")
;;; (tinydebian-bts-parse-string-1 "")
;;; (tinydebian-bts-parse-string-1 "")
(defun tinydebian-bts-parse-string-1 (str)
  "Parse STR and Return '(bug type package description)."
  (when (stringp str)
    ;;  Treat long "folded" subject like:
    ;;
    ;;  Subject: Bug#353588 acknowledged by developer (Re: Bug#353588: lintian:
    ;;     [add new rule] check debian/control::Description better ...
    ;;
    (setq str
          (replace-regexp-in-string "[\r\n]+" " " str))
    (multiple-value-bind (bug type package desc)
        (tinydebian-bts-parse-string-with-bug str)
      (when (and (not package)
                 desc)
        (multiple-value-bind (ret-pkg ret-desc)
            (tinydebian-bts-parse-string-with-package desc)
          (setq package ret-pkg
                desc    ret-desc)))
      (if (and (stringp desc)
               (string= desc ""))
          (setq desc nil))
      (if (and bug desc)
          (list bug type package desc)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-parse-string-current-line ()
  (let ((str (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position))))
    (tinydebian-bts-parse-string-1 str)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-parse-string-subject ()
  (let ((str (mail-fetch-field "Subject")))
    (when str
      (tinydebian-bts-parse-string-1 str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-package-name-current-line ()
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (when line
      (multiple-value-bind (bug package)
          (tinydebian-bug-string-parse-wnpp-alert line)
        package))))

;;; ----------------------------------------------------------------------
;;;
(defun my-debian-bug-package-name-any ()
  "Search package name."
  (or (tinydebian-bug-package-name-current-line)
      (tinydebian-bug-package-name-header-pool)
      (tinydebian-bug-package-name-header-package)
      (progn
        (multiple-value-bind (bug type-orig package description)
            (tinydebian-bts-parse-string-subject)
          package))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-gnus-summary-subject ()
  "In Gnus *Summary* buffer return current subject."
  (tinydebian-email-gnus-summary-mode-macro
   (mail-fetch-field "Subject")))

;;; ----------------------------------------------------------------------
;;;
(defun my-tinydebian-subject-any ()
  "Try to find subject for mail message."
  (or (tinydebian-gnus-summary-subject)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-email-subject-type-parse ()
  "Read BTS Subject and return '(TYPE SUBJECT)"
  (let* ((subject (mail-fetch-field "Subject")))
    (when subject
      ;;  Bug#292579: marked as done (RFP: miwm -- MIcroscopic Window
      (let (type subject bug)
        (when (string-match "\\(?: (?\\)\\([a-z]+\\):\\(.*\\)" subject)
          (setq type    (match-string 1 subject)
                subject (match-string 2 subject)))
        (setq bug
              (tinydebian-bug-nbr-string subject))
        (list type subject bug)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-browse-url (url &rest args)
  "Call `browse-url' and ignore ARGS."
  (browse-url url))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-lisp-only (url &optional bug)
  "Open HTTP connection to URL and read result.
  If BUG is set, then read specific BUG page and create buffer for it.
  If buffer already exists, do nothing."
  (ti::process-http-request url (not 'port) (not 'timeout)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-lynx-dump (url &optional mode)
  "Run lynx(1) with option -dump using URL.
  Optional MODE is hint to activate `tinydebian-bts-mode' on text buffer"
  ;;  For fast lookup, record the binary's full path
  (unless (get 'tinydebian-browse-url-lynx-dump 'done)
    (put 'tinydebian-browse-url-lynx-dump 'done t)
    (put 'tinydebian-browse-url-lynx-dump 'program (executable-find "lynx")))
  (let ((path (get 'tinydebian-browse-url-lynx-dump 'program)))
    (if (not path)
        (error "TinyDebian: [ERROR] `lynx' not found in PATH for %s" url)
      (tinydebian-with-buffer-macro tinydebian-:buffer-www
                                    (message "TinyDebian: Wait, accessing %s" url)
                                    (tinydebian-call-process path nil "-dump" url)
                                    (when mode
                                      (turn-on-tinydebian-bts-mode)
                                      (let ((font (tinydebian-url-page-font-lock-keywords mode)))
                                        (when (and font
                                                   (or tinydebian-:font-lock-mode
                                                       global-font-lock-mode))
                                          (setq font-lock-keywords font)
                                          (font-lock-mode 1))))
                                    (goto-char (point-min))
                                    (display-buffer (current-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-1 (url &optional mode)
  "Call `tinydebian-:browse-url-function' with URL.
  Optional MODE is hint to activate `tinydebian-bts-mode' on result buffer."
  (if tinydebian-:browse-url-function
      (funcall tinydebian-:browse-url-function url mode)
    (tinydebian-browse-url-browse-url url)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-browse-url-by-bug (bug &optional file)
  "Browse by BUG number. Optionally save bug report to FILE.
  A prefix argument in interactive mode prompts for FILE to save."
  (interactive
   (let* ((prev (get 'tinydebian-bug-browse-url-by-bug 'file))
          (dir  (if prev
                    (file-name-directory prev)))
          (nbr  (read-string "Browse URL by bug number: "
                             (tinydebian-bug-nbr-search)))
          (name (if current-prefix-arg
                    (read-file-name
                     (format "Save bug %s to file: " nbr)
                     dir
                     nil
                     nil
                     (format "%s.txt" nbr)))))
     (put 'tinydebian-bug-browse-url-by-bug 'file name)
     (list nbr name)))
  (when (or (not (stringp bug))
            (not (string-match "^[0-9]+$" bug)))
    (error "TinyDebian: Invalid bug number `%s'." bug))
  (let ((tinydebian-:browse-url-function tinydebian-:browse-url-function))
    (if file
        (setq tinydebian-:browse-url-function
              (function tinydebian-browse-url-lynx-dump)))
    (tinydebian-browse-url-1
     (format "http://bugs.debian.org/%s"
             (if (numberp bug)
                 (int-to-string bug)
               bug)))
    (if file
        (with-current-buffer (get-buffer tinydebian-:buffer-www)
          (write-region (point-min) (point-max) file)
          (if (interactive-p)
              (message "Wrote %s" file))
          file)
      tinydebian-:buffer-www)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-buffer-name (bug)
  (or bug
      (error "TinyDebian: BUG argument is empty"))
  (format tinydebian-:buffer-bug-format bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-debian-bugs (string)
  "Return bugs URL."
  (format "%s/%s" tinydebian-:url-http-package-bugs string))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-buffer-or-retrieve (bug)
  "Return buffer for BUG or send HTTP request to read bug.
  Return:
  buffer name"
  (or bug
      (error "TinyDebian: BUG argument is empty"))
  (let* ((name   (tinydebian-bug-buffer-name bug))
         (buffer (get-buffer name))
         (url    (tinydebian-url-debian-bugs bug)))
    (if buffer
        buffer
      (setq buffer (get-buffer-create name))
      (ti::process-http-request url (not 'port) (not 'timeout) buffer)
      buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-browse-url-by-package-name (package)
  "Jump to PACKAGE description."
  (interactive
   (list (read-string "Browse desription URL by package name: "
                      (my-debian-bug-package-name-any))))
  (when (or (not (stringp package))
            (not (string-match "[a-z]" package)))
    (error "TinyDebian: Invalid package name `%s'." package))
  (tinydebian-browse-url-1
   (tinydebian-packages-browse-url-compose package)
   package))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-browse-url-by-package-bugs (package)
  "Jump to PACKAGE description."
  (interactive
   (list (read-string "Browse bugs URL by package name: "
                      (my-debian-bug-package-name-any))))
  (when (or (not (stringp package))
            (not (string-match "[a-z]" package)))
    (error "TinyDebian: Invalid package name `%s'." package))
  (tinydebian-browse-url-1
   (tinydebian-url-debian-bugs package)
   package))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-show-wnpp-alert-format ()
  "Convert lines to more readable format from current point.

  Original:

  RFH 354176 cvs -- Concurrent Versions System
  O 367169 directvnc -- VNC client using the framebuffer as display

  After formatting:

  RFH 354176 cvs       -- Concurrent Versions System
  O   367169 directvnc -- VNC client using the framebuffer as display"
  (let ((re (concat
             "\\([a-z]+\\) +\\([0-9]+\\) +\\([^ \t\r\n]+\\)"
             " +-- +\\(.*\\)")))
    (while (re-search-forward re nil t)
      (replace-match (format "%-3s %d %-12s -- %s"
                             (match-string 1)
                             (match-string 2)
                             (match-string 3)
                             (match-string 4))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-show-wnpp-alert ()
  "Check for installed packages up for adoption or orphaned.
  Requires that program wnpp-alert(1) has been installed."
  (interactive)
  (let* ((bin  "wnpp-alert")
         (path (executable-find bin)))
    (cond
     ((not bin)
      (message "TinyDebian: [ERROR] program `%s' is not installed."
               bin))
     (t
      (tinydebian-with-buffer-macro tinydebian-:buffer-wnpp-alert
                                    (message "TinyDebian: wait, running %s..." path)
                                    (tinydebian-call-process path)
                                    (message "TinyDebian: wait, running %s... Done." path)
                                    (goto-char (point-min))
                                    (save-excursion
                                      (tinydebian-command-show-wnpp-alert))
                                    (turn-on-tinydebian-bts-mode)
                                    (display-buffer buffer)
                                    buffer)))))

;;}}}
;;{{{ BTS URL pages

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-with-url-page-type-macro 'edebug-form-spec '(body))
(put 'tinydebian-with-url-page-type-macro 'lisp-indent-function 1)
(defmacro tinydebian-with-url-page-type-macro (page-type &rest body)
  "Retrieve PAGE-TYPE from `tinydebian-:url-debian-page-alist' and run BODY.
  Variable `page'is bound to the retrieved value.
  Signal error if PAGE-TYPE is not found."
  `(let ((page (assoc ,page-type tinydebian-:url-debian-page-alist)))
     (unless page
       (error "TinyDebian: unknown page-typpe `%s'" ,page-type))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-page-compose (page-type)
  "Return URL location of PAGE-TYPE."
  (tinydebian-with-url-page-type-macro page-type (nth 1 page)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-page-font-lock-keywords (page-type)
  "Return `font-lock-keywords' of PAGE-TYPE."
  (tinydebian-with-url-page-type-macro page-type (nth 2 page)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-debian-mentors-url (package &optional section)
  "Return PACKAGE URL to mentors.debian.net in optional SECTION (def. main)."
  (let* ((first-char (substring package 0 1)))
    (format "%s/%s/%s/%s"
            (tinydebian-url-page-compose 'mentors-pkg-pool)
            (or section "main")
            first-char
            package)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-debian-browse-url (page-type &optional mode)
  "Browse Debian pages.
  Optional MODE is hint to activate `tinydebian-bts-mode' on result buffer."
  (let ((url (tinydebian-url-page-compose page-type)))
    (unless url
      (error "TinyDebian: Unknown URL request `%s'." page-type))
    (cond
     ((and (tinydebian-string-p url)
           (string-match "^/" url))
      (when (and (string-match "z$" url)
                 (null auto-compression-mode))
        (auto-compression-mode 1))
      (if (file-exists-p url)
          (find-file-other-window url)
        (error "TinyDebian: need 'apt-get install ...' (not found %s)"
               url)))
     ((string-match ":" url)
      (tinydebian-browse-url-1 url mode)))
    (t
     (error "TinyDebian: browse internal error `%s' `%s' `%s'"
            page-type mode url))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-bts-ctrl-page ()
  "Browse BTS control page."
  (interactive)
  (tinydebian-url-debian-browse-url 'bts-control))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-new-maintainer-guide ()
  "Browse Debian New Maintainers' Guide."
  (interactive)
  (tinydebian-url-debian-browse-url 'newmaint-guide))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-best-practises ()
  "Browse  Debian Developer's Reference Chapter 6 - Best Packaging Practices."
  (interactive)
  (tinydebian-url-debian-browse-url 'best-practices))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-developers-reference (&optional text-file)
  "Browse  Debian Developer's Reference.
  Optionally use TEXT-FILE from /usr/share/doc if found."
  (interactive "P")
  (tinydebian-url-debian-browse-url
   (if text-file
       'developers-reference-text
     'developers-reference)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-manual (&optional text-file)
  "Browse policy manual page.
  Optionally use TEXT-FILE from /usr/share/doc if found."
  (interactive "P")
  (tinydebian-url-debian-browse-url
   (if text-file
       'policy-text
     'policy)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-best-practises ()
  "Browse policy manual page: best practises section."
  (interactive)
  (tinydebian-url-debian-browse-url 'best-practises))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-bugs-by-rc ()
  "Browse release critical bugs."
  (interactive)
  (tinydebian-url-debian-browse-url 'bugs-rc 'bugs-rc))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-package-debcheck (package &optional distribution)
  "Check package for debcheck problems.
  Optionally from DISTRIBUTION which defaults to `testing'."
  (interactive
   (list
    (read-string "Debcheck package: ")
    (completing-read "Distribution: "
                     '(("stable" . 1)
                       ("testing" . 1)
                       ("unstable" . 1)
                       ("experimental" . 1))
                     (not 'predicate)
                     (not 'require-match))))
  (when (and (stringp package)
             (not (string= "" package)))
    (tinydebian-url-debian-browse-url-1
     (format (tinydebian-url-page-compose 'debcheck-package)
             (or distribution  "testing")
             package))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-qa-developer-status (email)
  "Browse QA developer status information by EMAIL address."
  (interactive
   (list (read-string "[QA status] developer's email address: "
                      (tinydebian-email-search))))
  (tinydebian-string-p
   email
   (format "[ERROR] email is missing from input [%s]" email))
  (tinydebian-browse-url-1
   (format "%slogin=%s" (tinydebian-url-page-compose 'qa-developer-status) email)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-qa-developer-bugs (email)
  "Browse QA developer bugs information by EMAIL address."
  (interactive
   (list (read-string "[QA bugs] developer's email address:"
                      (tinydebian-email-search))))
  (tinydebian-string-p
   email
   (format "[ERROR] email is missing from input [%s]" email))
  (tinydebian-browse-url-1
   (format "%ssubmitter=%s" (tinydebian-url-page-compose 'qa-developer-bugs) email)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-dsfg-license-faq ()
  "Browse DFSG FAQ about Licenses."
  (interactive)
  (tinydebian-browse-url-1 (tinydebian-url-page-compose 'dfsg-license-faq)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-base-files-faq ()
  "Browse base-files FAQ."
  (interactive)
  (tinydebian-browse-url-1 (tinydebian-url-page-compose 'base-files-faq)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-package-by-filename (filename &optional arch)
  "Package content search by FILENAME and optional ARCH."
  (interactive
   (let ((name (read-string "[Pkg search] filename: "))
         (arch (read-string "[Pkg search] architecture [RET=all]: ")))
     (list name arch)))
  (tinydebian-string-p
   filename
   (format "[ERROR] filename is missing from input [%s]" filename))
  ;; http://packages.debian.org/cgi-bin/search_contents.pl?word=svn_load_dirs&searchmode=searchfiles&case=insensitive&version=stable&arch=i386
  (tinydebian-browse-url-1
   (format "%s%s&word=%s"
           (tinydebian-url-page-compose 'pkg-search-files)
           (if (tinydebian-string-p arch)
               (format "&arch=%s" arch)
             "")
           filename)))

(defun tinydebian-grep-find-debian-devel (regexp grep-opt)
  "Grep REGEXP from all ddevelopment text files (policy etc.)"
  (interactive "sRegexp: \nsGrep opt (no single quotes): ")
  (let ((path-list (mapconcat
                    'concat
                    (delq nil
                          tinydebian-:grep-find-devel-docdir-list)
                    " "))
        cmd)
    (setq cmd
          (format
           (concat
            "find %s -type f -name '*.txt.gz' -print0 "
            "| xargs -0 -e zgrep -n %s '%s'")
           path-list
           grep-opt
           regexp))
    (grep-find cmd)))

;;}}}
;;{{{ WNPP URLs

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-wnpp-compose (page-type)
  "Return URL to search"
  (let ((page (assoc page-type tinydebian-:url-http-wnpp-page-alist)))
    (unless page
      (error "TinyDebian: unknow page-typpe `%s'" page-type))
    (format "%s/%s" tinydebian-:url-http-wnpp-page-main (cdr page))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-usertag-compose (tag)
  "Return URL to search"
  (format "%s/usertag:%s" tinydebian-:url-http-debian-www tag))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-wnpp-browse-url (page-type)
  "Browse WNPP PAGE-TYPE."
  (tinydebian-browse-url-1 (tinydebian-url-wnpp-compose page-type)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-bugs-by-usertag (usertag)
  "Browse by USERTAG."
  (interactive "sUsertag to search: ")
  (tinydebian-string-p
   usertag
   (format "[ERROR] usertag is missing from input [%s]" usertag))
  (tinydebian-browse-url-1 (tinydebian-url-usertag-compose usertag)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-itp ()
  "Browse WNPP ITP page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "ITP"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-rfp ()
  "Browse WNPP RFP page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "RFP"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-rfh ()
  "Browse WNPP RFH page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "RFH"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-rfa ()
  "Browse WNPP RFA page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "RFA"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-orphaned ()
  "Browse WNPP orphaned page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "O"))

;;}}}
;;{{{ BTS functions: Debian Developer interface to bug tracking system

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-insert-headers ()
  "Insert tinydebian-:bts-extra-headers' to mail buffer."
  (let ((headers tinydebian-:bts-extra-headers))
    (when (stringp headers)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward mail-header-separator nil t)
          (forward-line 0)
          (insert headers))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-bts-mail-compose-macro 'edebug-form-spec '(body))
(put 'tinydebian-bts-mail-compose-macro 'lisp-indent-function 5)
(defmacro tinydebian-bts-mail-compose-macro
  (bug type package subject email &rest body)
  "Compose mail with SUBJECT and run BODY."
  (let ((name (gensym "name-")))
    `(let ((,name (format "*Mail Debian BTS %s*"
                          (cond
                           ((and ,bug ,type ,package)
                            (format "%s %s %s"
                                    ,type ,package ,bug))
                           ((and ,bug ,package)
                            (format "%s %s"
                                    ,package ,bug))
                           (t
                            (or ,bug
                                ,subject
                                ""))))))
       (pop-to-buffer (get-buffer-create ,name))
       (erase-buffer)
       (mail-setup
        (if ,email
            ,email
          (tinydebian-bts-email-compose "control"))
        ,subject
        nil
        nil
        nil
        nil)
       (cond
        ((or (featurep 'message)
             (eq mail-user-agent 'message-user-agent))
         (message-mode))
        (t
         (mail-mode)))
       (tinydebian-bts-insert-headers)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-bts-mail-type-macro 'edebug-form-spec '(body))
(put 'tinydebian-bts-mail-type-macro 'lisp-indent-function 4)
(defmacro tinydebian-bts-mail-type-macro (type pkg email subject &rest body)
  "Compose a TYPE request and run BODY.
Variables available: bugnbr, type-orig, package, description; but these
can all be nil."
  (let ((subj (gensym "subject-")))
    `(multiple-value-bind (bugnbr type-orig package description)
         (or (tinydebian-bts-parse-string-current-line)
             (tinydebian-bts-parse-string-subject))
       (if (stringp ,pkg) ;; Use input argument
           (setq package ,pkg))
       (let ((,subj (or ,subject
                        (if ,type
                            (format "%s: %s%s"
                                    ,type
                                    (if package
                                        (format "%s -- " package)
                                      "")
                                    (or description ""))
                          ""))))
         (tinydebian-bts-mail-compose-macro
          bugnbr
          ,type
          package
          ,subj
          ,email
          (goto-char (point-max))
          ,@body)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-mail-ask-bug-number (&optional type)
  "Ask bug number. Return as '(bug) suitable for interactive"
  (read-string
   (format "Debian BTS %sbug number: "
           (if type
               (concat type " ")
             ""))
   (tinydebian-bug-nbr-any)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-ita (bug)
  "Send an ITA request."
  (interactive (list (tinydebian-bts-mail-ask-bug-number "ITA")))
  (tinydebian-bts-mail-type-macro "ITA" nil nil nil
                                  (insert
                                   (format "\
  retitle %s %s
  owner %s !
  thanks
  "
                                           bug
                                           (concat "ITA: "
                                                   (if package
                                                       (format "%s -- " package)
                                                     "")
                                                   (or description ""))
                                           bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-itp (bug)
  "Reposnd to RFP with an ITP request."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number "ITP response to RFP")))
  (tinydebian-bts-mail-type-macro "ITP" nil nil nil
                                  (insert
                                   (format "\
  retitle %s %s
  owner %s !
  thanks
  "
                                           bug
                                           (concat "ITP: "
                                                   (if package
                                                       (format "%s -- " package)
                                                     "")
                                                   (or description ""))
                                           bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-reply (bug)
  "Reply to bug found at current point or line"
  (interactive (list (tinydebian-bts-mail-ask-bug-number "Reply to bug")))
  (let ((subject (my-tinydebian-subject-any)))
    (tinydebian-bts-mail-compose-macro
     bug
     "reply"
     "bug"
     subject
     (tinydebian-bts-email-compose bug)
     (mail-position-on-field "CC")
     (insert (tinydebian-bts-email-compose (format "%s-submitter" bug)))
     (goto-char (point-max))
     nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-orphan (package license homepage desc)
  "Send an orphan request."
  (interactive)
  (message "tinydebian-bts-mail-type-orphan not yet implemented."))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-pkg-read-details-directory (directory)
  "Assuming a simgle debian package is in DIRECTORY, extract details.
  The directory should contain files:
  -rw-r--r-- 1 jaalto jaalto  19885 2006-11-19 18:12 pkg_0.2.4-4.diff.gz
  -rw-r--r-- 1 jaalto jaalto    605 2006-11-19 18:12 pkg_0.2.4-4.dsc
  -rw-r--r-- 1 jaalto jaalto   1106 2006-11-19 18:12 pkg_0.2.4-4_i386.changes
  -rw-r--r-- 1 jaalto jaalto 122188 2006-11-19 18:12 pkg_0.2.4-4_i386.deb
  -rw-r--r-- 1 jaalto jaalto    339 2006-11-19 18:12 pkg_0.2.4-4_i386.upload
  -rw-r--r-- 1 jaalto jaalto    942 2006-11-19 18:12 pkg_0.2.4-4_source.changes
  -rw-r--r-- 1 jaalto jaalto 246864 2006-11-19 18:12 pkg_0.2.4.orig.tar.gz

  RETURN:
  ((pkg-name       . \"pkg\")
   (pkg-ver-major  . \"0.2.4\")
   (pkg-ver-minor  . \"4\")
   (dsc            . \"pkg_0.2.4-4.dsc\")
   (deb            . \"pkg_0.2.4-4.dsc\")
   "
  (let* ()))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-rfs (package license bug desc)
  "Send an RFS request: PACKAGE name, package LICENCE and BUG and DESC.
   The DESC is short one line description string use in Subject."
  (interactive
   (let* ((name    (read-string
                    "RFP package name [required; lowercase]: ")) ;
          (license (tinydebian-read-license "License [required]: "))
          (bug      (read-string
                     "ITA/ITP bug number [required]: "))
          (desc    (read-string
                    "One line description [required]: ")))
     (list name license bug desc)))
  (flet ((replace (regexp str &optional point all)
                  (when (and (stringp str)
                             (not (string= "" str)))
                    (goto-char (or point
                                   (point-min)))
                    (if all
                        (while (re-search-forward regexp nil t)
                          (replace-match str 'literal nil nil 1))
                      (if (re-search-forward regexp nil t)
                          (replace-match str 'literal nil nil 1))))))
    (let* ((arg-pkg package) ;; Due to macro which reserves var `package'.
           (mentors-url (tinydebian-url-debian-mentors-url package))
           (ita-url     (tinydebian-url-debian-bugs bug))
           (pkg-url     (tinydebian-url-debian-bugs package)))
      (tinydebian-bts-mail-type-macro "RFS"
                                      arg-pkg (tinydebian-list-email-compose "debian-mentors") nil
                                      (insert tinydebian-:rfs-template)
                                      (replace "\\(<package>.*\\)"    package nil 'all)
                                      (replace "\\(<bugs:.*\\)"       pkg-url)
                                      (replace "\\(<ita:.*\\)"        ita-url)
                                      (replace "\\(<mentors:.*\\)"    mentors-url)
                                      (replace "\\(<license:.*\\)"    license)
                                      (mail-position-on-field "Subject")
                                      (beginning-of-line)
                                      (replace ": \\(.*\\)"
                                               (format "RFS: %s -- %s" package desc)
                                               (point))
                                      (goto-char (point-max))
                                      (run-hooks 'tinydebian-:rfs-hook)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-rfp (package license homepage desc)
  "Send an ITP request."
  (interactive
   (let* ((name    (read-string
                    "RFP package name [required; lowercase]: "))
          (desc    (read-string
                    "Package description [required]: "))
          (license (completing-read
                    "License [required]: "
                    (mapcar (lambda (x)
                              (cons x 1))
                            tinydebian-:wnpp-template-licenses-alist)))
          (url     (read-string
                    "Project homepage URL [required]: ")))
     (list name license url desc)))
  (flet ((replace (regexp str &optional point all)
                  (when (and (stringp str)
                             (not (string= "" str)))
                    (goto-char (or point
                                   (point-min)))
                    (if all
                        (while (re-search-forward regexp nil t)
                          (replace-match str 'literal nil nil 1))
                      (if (re-search-forward regexp nil t)
                          (replace-match str 'literal nil nil 1))))))
    (let ((arg-pkg package)) ;; Due to macro which reserves var `package'.
      (tinydebian-bts-mail-type-macro "ITP"
                                      arg-pkg (tinydebian-bts-email-submit) nil
                                      (insert tinydebian-:rfp-template)
                                      (replace "\\(<package>.*\\)"    package nil 'all)
                                      (replace "\\(<homepage:.*\\)"   homepage)
                                      (replace "\\(<license:.*\\)"    license)
                                      (replace "\\(<short desc>.*\\)" desc)
                                      (mail-position-on-field "Subject")
                                      (beginning-of-line)
                                      (replace ": \\(.*\\)"
                                               (format "RFP: %s -- %s" package desc)
                                               (point))
                                      (goto-char (point-max))
                                      (run-hooks 'tinydebian-:rfp-hook)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-severity (bug severity)
  "Compose BTS control message to a BUG and chnage SEVERITY."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         (completing-read
          "BTS severity: "
          tinydebian-:severity-list
          nil
          'match)))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s Change of severity / %s" bug severity)
   (insert
    (format "\
severity %s %s
thanks

"
            bug
            severity))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-usertag (bug &optional tag-string)
  "Compose BTS control message usertag to a BUG with TAG-STRING."
  (interactive
   (let ((bug (tinydebian-bts-mail-ask-bug-number)))))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s change of usertag %s" bug (or tag-string ""))
   (insert
    (format "\
usertag %s +
thanks
"
            bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-tags (bug tag-string)
  "Compose BTS control message to a BUG with TAG-STRING."
  (interactive
   (let ((bug (tinydebian-bts-mail-ask-bug-number))
         tag
         list)
     (while (or (null tag)
                (not (string= "" tag)))
       (setq tag (completing-read
                  "BTS tag [RET when done]: "
                  tinydebian-:tags-list
                  nil
                  'match))
       (unless (string= "" tag)
         (push tag list)))
     (list bug
           (mapconcat 'concat list " "))))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s change of tags / %s" bug tag-string)
   (insert
    (format "\
tags %s + %s
thanks

"
            bug
            tag-string))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-reassign (bug &optional package)
  "Compose BTS control message to a BUG amd reassign PACKAGE."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         (read-string "Reassign to package: ")))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s%s reassign " bug (if package
                                        (format " to package %s"
                                                package)
                                      ""))
   (insert
    (format "\
reassign %s %s
thanks

"
            bug
            (if (and package
                     (not (string= "" package)))
                package
              "<to-package>")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-retitle (bug title)
  "Compose BTS control message to a BUG and change TITLE."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         (read-string "New title: ")))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Reassign Bug#%s" bug)
   (insert
    (format "\
retitle %s %s
thanks

"
            bug
            title))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-reopen (bug)
  "Compose BTS control message a BUG and reopen it."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Reopen Bug#%s" bug)
   (insert
    (format "\
reopen %s !
thanks

"
            bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-close (bug &optional package version)
  "Compose BTS control message to close BUG.
Optional PACAGE name and VERSION number can be supplied."
  (interactive
   (let ((bug      (tinydebian-bts-mail-ask-bug-number))
         (package  (read-string "Package name [RET=ignore]: "))
         version)
     (if (tinydebian-string-p package)
         (setq version (read-string "Version: "))
       (setq package nil))
     (list bug
           package
           (if (tinydebian-string-p version)
               version
             nil))))
  (let* ((email (tinydebian-bts-email-compose (format "%s-done" bug)))
         (pkg   package))
    (tinydebian-bts-mail-type-macro
     nil
     pkg
     email
     (format "Bug#%s Close" bug)
     (insert
      (if (not (stringp package))
          ""
        (format "\
Package: %s
Version: %s
"
                package
                (or version "")))
      "\nReason for close:\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-forward-upstream (bug)
  "Compose BTS control message: forward BUG report to upstream."
  (let* ((email-forward (tinydebian-bts-email-compose
                         (format "%s-forwarded" bug)))
         (email-bug (tinydebian-bts-email-compose bug)))
    (tinydebian-bts-mail-type-macro
     nil nil "<upstream address>"
     (format "Debian Bug#%s -- forwarded upstream" bug)
     (mail-position-on-field "Cc")
     (insert (format "%s, %s" email-forward email-bug))
     (goto-char (point-max))
     (insert
      (format "\
\[Please keep the CC]

")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-forward-bts (bug)
  "Compose BTS forwarded control message to BTS."
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Debian Bug#%s -- forwarded upstream" bug)
   (insert
    (format "\
forwarded %s <http://upstream.example.com/bug-tracking/nbr>
thanks

"
            bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-forward-main (bug &optional control-message)
  "Compose BTS control message: forward BUG report to upstream.
If optional CONTROL-MESSAGE is non-nil, then compose regular BTS control
message which can be used to record upstream's bug tracking system URL."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         current-prefix-arg))
  (if control-message
      (tinydebian-bts-mail-ctrl-forward-bts bug)
    (tinydebian-bts-mail-ctrl-forward-upstream bug)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-message-info (bug &optional quiet)
  "Send more information to BUG, possibly with QUIET on.
With QUIET,  the  email will only be archived, sent to package maintainer
and not forwarded any Debian mailing lists."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         current-prefix-arg))
  (let* ((email (tinydebian-bts-email-compose
                 (if quiet
                     (format "%s-maintonly" bug)
                   bug))))
    (tinydebian-bts-mail-type-macro
     nil nil email
     (format "Debian Bug#%s " bug))))

;;}}}
;;{{{ Dpkg, apt functions

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-read-field-content-1 ()
  "Read content. Point must be positionioned at Field:-!-."
  (let* ((str (if (looking-at " +\\(.*\\)")
                  (match-string 1))))
    (while (and (not (eobp))
                (zerop (forward-line 1)) ;; Did it
                (looking-at "^\\( +.*\\)"))
      (setq str (concat (or str "") (match-string 1))))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-read-field-content (&optional field)
  "Read FIELD forward. FIELD ust be name like `Package'.
Be sure to call `tinydebian-package-narrow-to-region' first."
  (when (re-search-forward (format "^%s:" field) nil t)
    (tinydebian-package-read-field-content-1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-parse-info-all ()
  "Parse all fields forward. Return '((field . info) (field . info) ..)."
  (let* (field
         alist)
    (while (re-search-forward "^\\([^ \t\r\n]+\\):" nil t)
      (setq field (match-string 1))
      (push (cons field (tinydebian-package-read-field-content-1))
            alist))
    (nreverse alist)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-info-from-buffer (buffer)
  "Parse dpkg -s from BUFFER. Buffer must contain nothing else."
  (with-current-buffer buffer
    (goto-char (point-min))
    (tinydebian-package-parse-info-all)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-parse-depends-1 ()
  "Parse `Depends' field content from current point forward.
There must nothing else in the buffer."
  (let* (name
         op
         ver
         list)
    (while (re-search-forward "\\([a-z][^ ,()\t\r\n]+\\)" nil t)
      (setq name (ti::remove-properties (match-string 1))
            op   nil
            ver  nil)
      (cond
       ((looking-at " +(\\([=><]+\\) +\\([^ ,()\t\r\n]+\\))")
        (setq op   (ti::remove-properties (match-string 1))
              ver  (ti::remove-properties (match-string 2))))
       ((looking-at " *,?")))
      (goto-char (match-end 0))
      (push (list name op ver) list))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-parse-depends (depends)
  "Parse `Depends' field from DEPENDS string.
Example of the DEPENDS string:

    \"libc6 (>= 2.2.4-2), cron (>= 3.0pl1-42)\"

Returned list is

   '((\"libc6\" \">=\" \"2.2.4-2\")
     (\"cron\"  \">=\" \"3.0pl1-42\"))."
  (with-temp-buffer
    (insert depends)
    (ti::pmin)
    (tinydebian-package-status-parse-depends-1)))

;;; ----------------------------------------------------------------------
;;;
;;; #todo:
(defun tinydebian-package-status-apt-file (package)
  "Use apt-file PACKAGE (must be installed separately) to find upstream."
  (let* ((bin (executable-find "apt-file")))
    (cond
     ((null bin)
      (message "TinyDebian: no `apt-fil' found along PATH (emacs `exec-path').")
      (message "TinyDebian: Please run 'apt-get install apt-file'")
      nil)
     nil)))

;;; ----------------------------------------------------------------------
;;;
;;; Package: autolog
;;; Status: install ok installed
;;; Priority: extra
;;; Section: admin
;;; Installed-Size: 45
;;; Maintainer: Nicol�s Lichtmaier <nick@debian.org>
;;; Version: 0.35-10
;;; Depends: libc6 (>= 2.2.4-2), cron (>= 3.0pl1-42)
;;; Recommends: mail-transport-agent
;;; Conffiles:
;;;  /etc/autolog.conf a3fcae584ed74543a4a943e722593ff6
;;;  /etc/cron.d/autolog 805d268ea44c645299defc1c14495282
;;; Description: Terminates connections for idle users
;;;  Autolog terminates connections considered to be idle based on a large
;;;  variety of parameters.
;;;
(defun tinydebian-package-status-dpkg-s (package)
  "Consult dpkg -s PACKAGE"
  (let* ((dpkg tinydebian-:bin-dpkg))
    (cond
     ((not dpkg)
      (message "TinyDebian: no `dpkg' found along PATH (emacs `exec-path').")
      nil)
     (t
      (with-temp-buffer
        (message "TinyDebian: Running ... dpkg -s %s" package)
        (tinydebian-call-process dpkg nil "-s" package)
        (ti::pmin)
        (when (re-search-forward "^Use dpkg" nil t)
          (message "TinyDebian: `dpkg`-s %s' returned error [%s]"
                   package
                   (buffer-string)))
        (tinydebian-package-parse-info-all))))))

;;; ----------------------------------------------------------------------
;;; dpkg -S dh_make
;;;
;;; debhelper: /usr/bin/dh_makeshlibs
;;; dh-make: /usr/share/debhelper/dh_make/debian/postrm.ex
;;; dh-make: /usr/share/debhelper/dh_make/native
;;; dh-make: /usr/share/debhelper/dh_make/debian/changelog
;;; dh-make: /usr/share/debhelper/dh_make/debianl/shlibs.local.ex
;;; dh-make: /usr/share/man/man1/dh_make.1.gz
;;; dh-make: /usr/bin/dh_make
;;; dh-make: /usr/share/debhelper/dh_make/debiank/README.Debian
;;; dh-make: /usr/share/debhelper/dh_make/debianm/control
;;; dh-make: /usr/share/debhelper/dh_make/debian/init.d.ex
;;; dh-make: /usr/share/debhelper/dh_make/debian/cron.d.ex
;;; dh-make: /usr/share/debhelper/dh_make/debianm/rules
;;; dh-make: /usr/share/debhelper/dh_make/licenses/lgpl
;;; dh-make: /usr/share/debhelper/dh_make/debiank/control
;;; dh-make: /usr/share/debhelper/dh_make/debians/rules
;;; dh-make: /usr/share/debhelper/dh_make/debianl/package1.dirs
;;; dh-make: /usr/share/debhelper/dh_make/native/changelog
;;; dh-make: /usr/share/debhelper/dh_make/licenses/bsd
;;; dh-make: /usr/share/debhelper/dh_make/debianm/package-doc.files
;;; dh-make: /usr/share/debhelper/dh_make/debians/watch.ex
;;; dh-make: /usr/share/debhelper/dh_make/licenses/gpl
;;; dh-make: /usr/share/debhelper/dh_make/licenses/blank
;;;
(defun tinydebian-package-status-dpkg-S-parse (package)
  "Examine dpkg -S PACKAGE listing and return package name."
  (ti::pmin)
  (when (re-search-forward (concat "^\\([^: \t\r\n]+\\):.*/"
                                   package
                                   "[ \t]*$")
                           nil t)
    (match-string 1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-dpkg-S (file)
  "Consult dpkg -S FILE
In this case, the package is unknown."
  (let* ((dpkg  tinydebian-:bin-dpkg))
    (cond
     ((not dpkg)
      (message "TinyDebian: no `dpkg' found along PATH (emacs `exec-path').")
      nil)
     (t
      (with-temp-buffer
        (message "TinyDebian: Running ... dpkg -S %s (takes a while)" file)
        (apply 'tinydebian-call-process dpkg nil (list "-S" file))
        (let ((pkg (tinydebian-package-status-dpkg-S-parse file)))
          (cond
           ((null pkg)
            (message
             "TinyDebian: dpkg -S doesn't know file `%s'" file)
            nil)
           (t
            (tinydebian-package-status-dpkg-s pkg)))))))))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun tinydebian-package-status-apt-cache (package)
  "Consult dpkg -S FILE
In this case, the package is unknown."
  (with-temp-buffer
    (message "TinyDebian: Running ... apt-cache show %s (takes a while)"
             package)
    (apply 'tinydebian-call-process "apt-cache" nil (list "show" package))
    (message "Done.")
    (unless (eq (point-max) (point-min))
      (goto-char (point-min))
      (tinydebian-package-parse-info-all))))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun tinydebian-package-status-grep-available (package)
  "Consult grep-available(1) for PACKAGE from 'Provides' field."
  (let* ((bin tinydebian-:bin-grep-available)
         (re  (format ".*[ ,]+%s([, \t]|[ \t]*$)" package)))
    (cond
     ((not bin)
      (message (concat "TinyDebian: no `grep-available' "
                       "found along PATH (emacs `exec-path')."))
      nil)
     (t
      (with-temp-buffer
        (message "TinyDebian: Running ... grep-available -e %s" package)
        (apply 'tinydebian-call-process
               bin
               nil
               (list "--field=Provides"
                     "--eregex"
                     re))
        (let* ((info (tinydebian-package-info-from-buffer (current-buffer))))
          (cond
           ((null info)
            (message
             "TinyDebian: grep-available doesn't know package`%s'" package)
            nil)
           (t
            info))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-wnpp-main-interactive ()
  "Ask the type of request for WNPP package.
References:
  `tinydebian-:menu-wnpp'
  `tinydebian-:menu-wnpp-selected'"
  (setq tinydebian-:menu-wnpp-selected nil)
  (ti::menu-menu 'tinydebian-:menu-wnpp)
  tinydebian-:menu-wnpp-selected)

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-buffer-ask-input (message buffer &optional clear)
  "Write MESSAGE to the buffer ans ask user to type input.
The MESSAGE should contgain properly formatted text."
  (let* ((buffer (ti::temp-buffer buffer clear)))))
    ;; (switch-to-buffer buffer)
    ;; #todo:

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-wnpp-main (request-type)
  "Submit REQUEST-TYPE against WNPP pseudo package.
WNPP is used for requesting to be a new Debian maintainer and
for taking maintenance of other packages. Refer to
http://www.debian.org/devel/wnpp and
http://www.debian.org/doc/packaging-manuals/developers-reference/ch-pkgs.en.html
and topic \"5.1 New Packages\"

REQUEST-TYPE can be symbol:

  'package 'orphan 'adopt or 'new.
  See http://www.debian.org/devel/wnpp for more information

References:

  `tinydebian-:menu-wnpp'."
  (interactive (list (tinydebian-package-wnpp-main-interactive)))
  (cond
   ((eq request-type 'package)
    (call-interactively 'tinydebian-bts-mail-type-itp))
   ((eq request-type 'new)
    (call-interactively 'tinydebian-bts-mail-type-rfp))
   ((eq request-type 'orphan)
    (call-interactively 'tinydebian-bts-mail-type-orphan))
   ((eq request-type 'adopt)
    (call-interactively 'tinydebian-bts-mail-type-ita))
   (t
    ;;  Nothing to do
    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-main (package)
  "Find out PACKAGE details."
  (or (tinydebian-package-status-apt-cache package)
      (tinydebian-package-status-dpkg-s package)
      (tinydebian-package-status-grep-available package)
      (tinydebian-package-status-dpkg-S package)
      (tinydebian-package-status-apt-file package)
      (if (string-match "^wnpp" package)
          (error (concat "TinyDebian: package WNPP is special. "
                         "Use tinydebian-package-wnpp-main instead.")))
      (error "Tinydebian: Can't find package information. `%s'" package)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-info (&optional package prompt)
  "Get PACKAGE information. See`tinydebian-package-status'.
If PACKAGE is nil and `tinydebian-:bin-dpkg' is not available,
ask with PROMPT."
  (let* ((dpkg  tinydebian-:bin-dpkg))
    (or package
        (setq package (read-string
                       (or prompt
                           "[TinyDebian] Package name: "))))
    (or (and dpkg
             (tinydebian-package-status-main package)))))
        ;; FIXME: todo

;;}}}
;;{{{ Bug reporting interface

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-general ()
  "Return relevant system information."
  ;; FIXME: todo
  (interactive))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-depends (info &optional depend-key)
  "Return additional Dependency INFO from item `Depends'.
DEPEND-KEY can be \"Depends\" or \"Pre-Depends\".

Example:

  Versions of packages autolog depends on:
  ii  cron            3.0pl1-72  management of regular background p
  ii  libc6           2.2.5-3    GNU C Library: Shared libraries an."
  (let* ((depends (cdr-safe (and info
                                 (assoc
                                  (or depend-key "Depends")
                                  info))))
         str)
    (when depends
      (setq str "")
      (dolist (dep-info
               (tinydebian-package-status-parse-depends depends))
        (multiple-value-bind (package op version)
            dep-info
          ;; Not used yet, quiet byte compiler
          (if op
              (setq op op))
          (if version
              (setq version version))
          (let* (info2
                 desc
                 ver)
            (setq info2
                  (tinydebian-package-info
                   package
                   (format "\
\[TinyDebian] Depend. Insert `dpkg -s %s' to *scratch* and press RET: "
                           package)))
            (setq ver  (cdr-safe (assoc "Version" info2)))
            ;; cut first few characters
            (when (setq desc (cdr-safe (assoc "Description" info2)))
              (setq desc (ti::string-left desc 45)))
            (setq str
                  (concat
                   str
                   (format "%-15s %-15s %s\n" package ver desc)))))))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os-architecture ()
  "Read architecture."
  (if (not tinydebian-:bin-dpkg)
      ""
    (with-temp-buffer
      (tinydebian-call-process
       tinydebian-:bin-dpkg  nil "--print-installation-architecture")
      (tinydebian-string-delete-newlines
       (buffer-string)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os-version ()
  "Read Debian version number."
  (let* ((file  "/etc/debian_version")
         (ret    (format "%s not found or readable." file)))
    (when (and (file-exists-p   file)
               (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (setq ret
              (tinydebian-string-delete-newlines
               (buffer-string)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-locale ()
  "Get locale information."
  (let* ((list
          '("LC_ALL"
            "LC_CTYPE"))
         val
         ret)
    (dolist (var list)
      (when (setq val (getenv var))
        (setq val (format "%s=%s" var val))
        (setq ret (if (null ret)
                      val
                    (concat ret ", " val)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os ()
  "Return OS information.
Debian Release: 3.0
Architecture: i386
Kernel: Linux terra 2.4.17 #1 Fri Feb 8 21:32:43 EET 2002 i686
Locale: LANG=en_US, LC_CTYPE=en_US."
  (let* ((kernel       (tinydebian-string-delete-newlines
                        (ti::process-uname)))
         (architecture (tinydebian-bug-system-info-os-architecture))
         (release      (tinydebian-bug-system-info-os-version))
         (locale       (tinydebian-bug-system-info-locale)))
    (format "\
Debian Release: %s
Architecture: %s
Kernel: %s
Locale: %s"
            release
            architecture
            kernel
            locale)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-severity ()
  "Select bug severity."
  (setq tinydebian-:severity-selected nil)
  (while (null tinydebian-:severity-selected)
    (ti::menu-menu 'tinydebian-:menu-severity)
    (unless tinydebian-:severity-selected
      (message "TinyDebian: Please select severity.")
      (sit-for 1)))
  tinydebian-:severity-selected)

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-report-mail-insert-details (info)
  "Insert Details for apckage INFO into Mail."
  (ti::mail-text-start 'move)
  (insert "Package: " (cdr (assoc "Package" info)) "\n")
  (insert "Version: " (cdr (assoc "Version" info)) "\n")
  (insert "Severity: " (tinydebian-bug-severity)   "\n\n")
  (let* ((point       (point))
         (depends     (tinydebian-bug-system-info-depends info "Depends"))
         (pre-depends (tinydebian-bug-system-info-depends info "Pre-Depends"))
         (package     (or (and info
                               (cdr (assoc "Package" info)))
                          (error "No package information."))))
    (insert "\n\n-- System Information\n"
            (tinydebian-bug-system-info-os)
            (format "\n\n-- Versions of packages `%s depends on'.\n"
                    package)
            (if pre-depends
                (concat "Pre-Depends:\n" pre-depends)
              "")
            (if depends
                (concat "Depends:\n" depends)
              ""))
    (goto-char point)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydebian-bug-report-mail (info)
  "Submit Debian bug report. INFO is alist of attributes for package.
An example �reportbug(1)' looks like

To: submit@bugs.debian.org
Subject: autolog ....
--text follows this line--
Package: autolog
Version: 0.35-10
Severity: wishlist

-- System Information
Debian Release: 3.0
Architecture: i386
Kernel: Linux foo 2.4.17 #1 Fri Feb 8 21:32:43 EET 2002 i686
Locale: LANG=en_US, LC_CTYPE=en_US

Versions of packages autolog depends on:
ii  cron                          3.0pl1-72  management of regular background p
ii  libc6                         2.2.5-3    GNU C Library: Shared libraries an

Subject: autolog based on DNS and IP names
Package: autolog
Version: 0.35-10
Severity: wishlist

-- System Information
Debian Release: 3.0
Architecture: i386
Kernel: Linux terra 2.4.17 #1 Fri Feb 8 21:32:43 EET 2002 i686
Locale: LANG=en_US, LC_CTYPE=en_US

Versions of packages autolog depends on:
ii  cron                          3.0pl1-72  management of regular background p
ii  libc6                         2.2.5-3    GNU C Library: Shared libraries an."
  (interactive
   (progn
     (if (y-or-n-p "[TinyDebian] Submit bug report? ")
         (list (tinydebian-package-info))
       nil)))
  (let ((status  (or (cdr-safe (assoc "Status" info)) ""))
        (package (or (cdr-safe (assoc "Package" info)) "")))
    (cond
     ((null info)
      (message "TinyDebian: no INFO available to send a bug report."))
     ((string-match "not-installed" status)
      (message "TinyDebian: bug report skipped. �%s' status is [%s]"
               package status))
     (t
      (let* ((name   (format "*mail* Debian Bug %s" package))
             buffer)
        (cond
         ((and (setq buffer (get-buffer name))
               (null (y-or-n-p
                      "[TinyDebian] delete previous bug report? ")))
          (pop-to-buffer buffer))
         (t
          (pop-to-buffer (get-buffer-create name))
          (erase-buffer)
          (let ((subject (read-string "[TinyDebian] bug Subject: ")))
            (mail-setup
             (tinydebian-bts-email-submit) subject nil nil nil nil))
          (message-mode)
          (tinydebian-bts-insert-headers)
          (tinydebian-bug-report-mail-insert-details info))))))))

;;}}}
;;{{{ Admin functions: MAIL reports

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-audit-report-tiger-make-chmod (file line)
  "Make suotable chmod command for FILE according to LINE report."
  (let* ((operand "+")
         group
         group-cmd
         type
         type-cmd)
    (when (string-match
           "should .*+have +\\([^ \t\r\n]+\\) +\\([^ \t\r\n.]+\\)"
           line)
      (setq group (match-string 1 line)
            type  (match-string 2 line))
      (if (string-match "should not" line)
          (setq operand "-"))
      (cond
       ((string= group "group")
        (setq group-cmd "g"))
       ((string= group "world")
        (setq group-cmd "o")))
      (cond
       ((string-match type "read")
        (setq type-cmd "r"))
       ((string-match type "write")
        (setq type-cmd "w"))
       ((string-match type "exec")
        (setq type-cmd "x")))
      (when (and operand type-cmd group-cmd)
        (format "chmod %s%s%s %s;" group-cmd operand type-cmd file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-audit-report-tiger (beg end)
  "Process tiger(1) mail system report on region BEG END.
The body of mail looks like:

    # Performing check of system file permissions...
    OLD: --WARN-- [perm001w] /var/log/wtmp should not have group write.
    OLD: --WARN-- [perm001w] /var/run/utmp should not have group write.
    OLD: --WARN-- [perm001w] /var/log/XFree86.0.log should not have world read.

For which a corresponding command to correct the error is generated.

    chmod g-w /var/log/wtmp;
    chmod g-w /var/run/utmp;
    chmod o-r /var/log/XFree86.0.log;

You can select region and these commands to shell `sh' with command
`shell-command-on-region' which can be called with \\[shell-command-on-region]."
  (interactive "r")
  (let* ((buffer (get-buffer-create tinydebian-:buffer-tiger))
         done
         file
         str)
    (goto-char beg)
    (while (re-search-forward
            "--WARN-- +[^ \t\r\n]+ +\\(\\([^ \t\r\n]+\\).*\\)"
            nil end)
      (setq file (match-string 2)
            str  (match-string 1))
      (unless done                  ;Draw one empty line between calls
        (setq done t)
        (ti::append-to-buffer buffer "\n"))
      (when (setq str (tinydebian-command-audit-report-tiger-make-chmod
                       file str))
        (ti::append-to-buffer buffer (concat str "\n"))))
    (cond
     ((ti::buffer-empty-p buffer)
      (message
       "TinyDebian: Hm, region did not have --WARN-- chmod candidates."))
     (t
      (display-buffer buffer)
      (message
       (substitute-command-keys
        (concat
         "TinyDebian: [tiger] "
         "Select region and send commands to"
         " `sh' with \\[shell-command-on-region]")))))))

;;}}}

(tinydebian-install-severity-functions) ;; Auto-created functions

(add-hook 'tinydebian-:bts-mode-define-keys-hook
          'tinydebian-bts-mode-define-keys)

(defalias 'tinydebian-reportbug 'tinydebian-bug-report-mail)

(provide   'tinydebian)
(run-hooks 'tinydebian-:load-hook)

;;; tinydebian.el ends here
