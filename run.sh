ghc MyRandom.hs Main.hs -o ./bin/uri-generator && ./bin/uri-generator $@
rm *.hi *.o
