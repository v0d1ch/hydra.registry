{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
project ./. ({pkgs,  hackGet, ... }: {
  packages = let 
    lensAesonSrc = hackGet ./dep/lens-aeson;
  in {
    lens-aeson = lensAesonSrc + "/lens-aeson";
  };
  android.applicationId = "systems.devnull.hydra.registry";
  android.displayName = "Hydra Head Registry";
  ios.bundleIdentifier = "systems.devnull.hydra.registry";
  ios.bundleName = "Hydra Head Registry";
})
