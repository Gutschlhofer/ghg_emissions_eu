system("mpstat")

memfree <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))

print(memfree)