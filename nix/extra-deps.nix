let
  dontCheck = (import ./release.nix).haskell.lib.dontCheck;
  doJailbreak = (import ./release.nix).haskell.lib.doJailbreak;
in (super: {
polysemy-plugin          = dontCheck (super.polysemy-plugin);
webdriver                = dontCheck (super.webdriver);
})
