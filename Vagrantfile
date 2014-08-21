# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"
ENV['VAGRANT_DEFAULT_PROVIDER'] = 'docker'

FLIPSTONE_DOCKER_FILE = "#{ENV['HOME']}/.flipstone_docker_host/Vagrantfile"

if ARGV.include?('up') &&
   !ARGV.include?('--no-parallel')
  exec "vagrant #{ARGV.join(' ')} --no-parallel"
end

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.define "db" do |db|
    db.vm.provider "docker" do |d|
      d.name    = "glados_db"
      d.cmd     = ["/sbin/my_init", "--enable-insecure-key"]
      d.image   = "flipstone/playground-postgresql"
      d.has_ssh = true

      if File.exists? FLIPSTONE_DOCKER_FILE
        d.vagrant_vagrantfile = FLIPSTONE_DOCKER_FILE
      end
    end

    [
      'createuser glados',
      'createdb -O glados glados_dev',
      'psql -c "ALTER USER \"glados\" WITH PASSWORD \'glados\';"',
    ].each do |command|
      db.vm.provision :shell, privileged: false, inline: command
    end
  end

  config.vm.define "app", primary: true do |api|
    api.vm.provider "docker" do |d|
      d.link 'glados_db:db'
      d.name    = "glados_app"
      d.cmd     = ["/sbin/my_init", "--enable-insecure-key"]
      d.image   = "flipstone/haskell-psql-client"
      d.has_ssh = true
      d.ports = [ "8000:8000" ]

      if File.exists? FLIPSTONE_DOCKER_FILE
        d.vagrant_vagrantfile = FLIPSTONE_DOCKER_FILE
      end
    end
  end

  config.vm.boot_timeout = 10
  config.ssh.private_key_path = "phusion.key"
end
