Name:           korektor
Version:        0.1.0.0
Release:        1%{?dist}
Summary:        Linter and language server for RPM spec files

License:        GPL-3.0-or-later
URL:            https://github.com/terrapkg/korektor
Source0:        %{url}/archive/refs/tags/v%{version}.tar.gz

BuildRequires:  ghc-rpm-macros
BuildRequires:  ghc-Cabal-devel
BuildRequires:  ghc-aeson-devel
BuildRequires:  ghc-base-devel
BuildRequires:  ghc-bytestring-devel
BuildRequires:  ghc-containers-devel
BuildRequires:  ghc-directory-devel
BuildRequires:  ghc-filepath-devel
BuildRequires:  ghc-lens-devel
BuildRequires:  ghc-lsp-devel
BuildRequires:  ghc-lsp-types-devel
BuildRequires:  ghc-mtl-devel
BuildRequires:  ghc-optparse-applicative-devel
BuildRequires:  ghc-rozbir-devel
BuildRequires:  ghc-text-devel
BuildRequires:  ghc-time-devel
BuildRequires:  ghc-toml-parser-devel
BuildRequires:  ghc-hspec-devel
BuildRequires:  ghc-temporary-devel

Packager:       Cypress Reed <cypress@fyralabs.com>

%description
korektor lints RPM spec files with rozbir and serves the same diagnostics,
fixes, hovers and completions over the language server protocol.


%prep
%autosetup


%build
%ghc_bin_build


%install
%ghc_bin_install


%check
%cabal_test


%files
%license LICENSE.md
%doc README.md
%{_bindir}/%{name}


%changelog
* Fri Nov 20 2026 Cypress Reed <cypress@fyralabs.com> - 0.1.0.0-1
- initial package
