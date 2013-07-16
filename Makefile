EMACS = emacs
CARTON = carton
VAGRANT = vagrant

elpa:
	$(CARTON) install
	$(CARTON) update

.PHONY: test
test: elpa
	script/test.sh

.PHONY: vagrant-test
vagrant-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /vagrant clean test"
	$(VAGRANT) destroy --force

.PHONY: clean
clean :
	rm -rf elpa emamux-ruby-test.elc
