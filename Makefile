SHELL = bash

repl:
	 rm -rf .cpcache/ && DEBUG=true && clojure -A:test:nrepl -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"
