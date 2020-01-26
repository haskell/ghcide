# This is a sample hie.yaml file for opening ghcide in
# ghcide/hie/ide, using cabal as the build system.
# To use is, copy it to a file called 'hie.yaml'

cradle:
  cabal:

    - path: "./test"
      component: "ghcide:ghcide-test"

    - path: "./exe"
      component: "ghcide:exe:ghcide"

    - path: "./src"
      component: "lib:ghcide"
