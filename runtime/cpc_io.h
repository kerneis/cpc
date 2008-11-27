int cpc_setup_descriptor(int fd, int nonagle);
cps int cpc_write(int fd, void *buf, size_t count);
cps int
cpc_write_timeout(int fd, void *buf, size_t count, int secs, int micros);
cps int cpc_read(int fd, void *buf, size_t count);
cps int
cpc_read_timeout(int fd, void *buf, size_t count, int secs, int micros);

