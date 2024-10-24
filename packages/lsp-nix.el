;;; lsp-nix.el --- lsp-mode nix integration    -*- lexical-binding: t; -*-

;; Author: Nick Breitling <breitling.nw@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Client for the nixd language server.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-nix-nixd nil
  "LSP support for Nix, using nixd language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/nix-community/nixd"))

(defcustom lsp-nix-nixd-server-path "nixd"
  "Executable path for the server."
  :group 'lsp-nix-nixd
  :type 'string
  :package-version '(lsp-mode . "8.0.0"))

(lsp-defcustom lsp-nix-nixd-formatting-command nil
  "External formatter command with arguments.

  Example: [ nixpkgs-fmt ]."
  :type 'lsp-string-vector
  :group 'lsp-nix-nixd
  :lsp-path "nixd.formatting.command"
  :package-version '(lsp-mode . "9.0.0"))

(lsp-defcustom lsp-nix-nixd-nixpkgs-expr nil
  "This expression will be interpreted as \"nixpkgs\" toplevel
  Nixd provides package, lib completion/information from it.

  Resource Usage: Entries are lazily evaluated, entire nixpkgs takes 200~300MB
  for just \"names\". Package documentation, versions, are evaluated by-need.

  Example:
  \"import <nixpkgs { }\""
  :type 'string
  :group 'lsp-nix-nixd
  :lsp-path "nixd.nixpkgs.expr"
  :package-version '(lsp-mode . "9.0.0"))

(lsp-defcustom lsp-nix-nixd-nixos-options-expr nil
  "Option set for NixOS option completion. If this is omitted, the default
  search path (<nixpkgs>) will be used.

  Example:
  \"(builtins.getFlake \"/home/lyc/flakes\").nixosConfigurations.adrastea.options\""
  :type 'string
  :group 'lsp-nix-nil
  :lsp-path "nixd.options.nixos.expr"
  :package-version '(lsp-mode . "9.0.0"))

(lsp-defcustom lsp-nix-nixd-home-manager-options-expr nil
  "Option set for home-manager option completion.

  Example:
  \"(builtins.getFlake \"/home/lyc/flakes\").nixosConfigurations.adrastea.options\""
  :type 'string
  :group 'lsp-nix-nil
  :lsp-path "nixd.options.home-manager.expr"
  :package-version '(lsp-mode . "9.0.0"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda () lsp-nix-nixd-server-path))
                  :major-modes '(nix-mode nix-ts-mode)
                  :server-id 'nixd-lsp-improved
                  :priority 1))

(lsp-consistency-check lsp-nix)

(provide 'lsp-nix)
;;; lsp-nix.el ends here
