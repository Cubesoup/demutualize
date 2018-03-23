I've set this up as a stack project, so to build it you have to:

+ have 'stack' installed
+ clone this repository
+ run 'stack build' in the directory that creates

This should ensure you have all the dependencies. The process builds an executable, but it doesn't do anything yet. The two interesting parts are in src/Parser.hs (the parser) and src/Stuff.hs (everything else). To see it go load Stuff.hs with 'stack ghci', and run one of the examples via 'runExample <exampleName>'. The included examples are named 'treeForest', 'fgh', 'onlyList' and 'multi'. 
