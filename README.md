# This is implementation of grep -r in erlang using multithread approach
run `erlgrep` escript to see how to use

# warning
this is not complete!

# sad panda ;/
small sets < 1k files -> im on a speed level of grep -r not faster ;/
big sets :D my ruby filder which has ~3gb of scripts and projects >1k files erlgrep ownzzz

      grep -r "controller" ~/Workspace/Ruby/* >>| /dev/null  0,72s user 4,79s system 2% cpu 3:06,83 total
      (and he did crash ! lulz )
      ./erlgrep "controller" ~/Workspace/Ruby/ >>| /dev/null  0,77s user 0,36s system 9% cpu 11,636 total
      (managed to finish and do whole thing)