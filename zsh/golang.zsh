if [ -d $HOME/go ]
then
else
	mkdir -p $HOME/go/bin
fi

export PATH=$PATH:/usr/local/go/bin:/home/xena/prefix/go/bin
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export GO15VENDOREXPERIMENT=1
