{ config, pkgs, ... }:
let
  dotDir = name: path: {
    ${name} = {
      source = path;
      recursive = true;
    };
  };
  myEmacs = pkgs.emacs.pkgs.withPackages (epkgs: [
    epkgs.vterm
  ]);

in
{
  imports = [
    ./modules/suckless.nix
  ];
  home.username = "xsoder";
  home.homeDirectory = "/home/xsoder";
  home.stateVersion = "25.05";
  programs.git.enable = true;
  programs.bash.enable = true;
  programs.starship.enable = true;
  home.file =
    dotDir ".emacs.local" ./dotfiles/emacs/emacs.local
    // dotDir ".vim/local" ./dotfiles/vim
    // dotDir ".emacs.rc" ./dotfiles/emacs/emacs.rc
    // {
      ".config/i3/config".source = ./dotfiles/i3/config;
      ".config/alacritty/alacritty.toml".source = ./dotfiles/alacritty/alacritty.toml;
      ".config/dunst/dunstrc".source = ./dotfiles/dunst/dunstrc;
      ".vimrc".source = ./dotfiles/vimrc;
      ".emacs".source = ./dotfiles/emacs/emacs;
      ".emacs.custom".source = ./dotfiles/emacs/emacs.custom.el;
      ".xprofile".source = ./dotfiles/xprofile;
    };
  home.packages = with pkgs; [
    firefox
    neofetch
    feh
    i3status
    myEmacs
    discord
    btop
    scrot
    dunst
    libnotify
    starship
  ];
}
