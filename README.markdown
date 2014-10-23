Development
-----------
Download https://github.com/flipstone/flipstone_docker_host if you don't have a local Docker host
`vagrant up`
`vagrant ssh`
`cabal update`
`cd /vagrant`
`cabal sandbox init`
`cabal install`
`cabal repl`
`main`
Navigate your browser to http://172.27.27.27:8000/


Configuration
-------------
In the event that the main process fails to start with a postgres db error:
`*** Exception: libpq: failed (FATAL:  password authentication failed for user "glados"`
`$ vagrant provision db`

Running II
----------
Download and install to Vagrant.
Start with:
`ii -i /tmp/irc -n vagrantFlippy -s chat.freenode.net`