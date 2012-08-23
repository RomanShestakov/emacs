import time
import stackless

def func():
    for i in range(120):
        time.sleep(1)
        stackless.schedule()

for i in range(2000):
    stackless.tasklet(func)()

stackless.run()
