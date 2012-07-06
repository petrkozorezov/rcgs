REBAR=./rebar
DIALYZER=dialyzer

.PHONY: all compile deps clean distclean doc dialyzer

all: compile

$(REBAR):
	wget https://github.com/downloads/basho/rebar/rebar -O $(REBAR)
	chmod +x $(REBAR)

deps: $(REBAR)
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

doc: compile
	$(REBAR) doc skip_deps=true

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps
	rm -rfv plts

release: compile
	cd rel && ../$(REBAR) generate

## dialyzer
## you have to compile project first
plts/otp.plt: ~/.dialyzer_plt
	mkdir -p plts && cp ~/.dialyzer_plt plts/otp.plt

plts/deps.plt: plts/otp.plt
	$(DIALYZER) --add_to_plt --plt plts/otp.plt --output_plt plts/deps.plt -r deps

dialyzer: plts/deps.plt
	rm -rf `find apps -name ".eunit"`
	$(DIALYZER) --plt plts/deps.plt -n -r apps

#\
#	-Wunmatched_returns -Werror_handling -Wrace_conditions -Wno_fun_app \
#	-Wunderspecs -Wno_opaque -Wno_return -Wno_unused -Wno_improper_lists

devrel: dev1 dev2 dev3

devclean:
	rm -rf dev

dev1 dev2 dev3: compile
	mkdir -p dev
	(cd rel && rm -rf ../dev/build_$@ && ../rebar generate target_dir=../dev/build_$@ overlay_vars=vars/$@.config && rsync ../dev/build_$@/* ../dev/$@ -rc)
