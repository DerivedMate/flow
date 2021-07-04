for _ in {0..5}
  do 
    /usr/bin/time -f "%E" deno run trans/js/base.mjs 
done
