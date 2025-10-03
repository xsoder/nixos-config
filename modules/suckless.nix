{ config, pkgs, suckless-builds, ... }:

let
  sucklessPkgs = suckless-builds.packages.${pkgs.system};
in
{
  home.packages = [
    sucklessPkgs.dmenu
    sucklessPkgs.st
    sucklessPkgs.dwm
  ] ++ (with pkgs; [
    (nerdfonts.override { fonts = [ "Iosevka" ]; })
  ]);

  fonts.fontconfig.enable = true;
}
