VARIANTS=base js # links tests
IMAGES=$(foreach variant,$(VARIANTS),links-handlers-$(variant))

all: $(IMAGES)

links-handlers-%:
	docker build -t $@ -f Dockerfile.$* .

.PHONY: links-handlers-%
