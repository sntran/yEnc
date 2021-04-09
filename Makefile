
# Generates binary file of certain size with random data.
test/fixtures/binaries/%.bin:
	dd if=/dev/urandom of=$@ bs=$* count=1
