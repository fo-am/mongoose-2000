import smbus
import os

bus = smbus.SMBus(1)

# if we are on battery power
if bus.read_byte_data(0x69,0)==2:
    # shut down nicely
    os.system("sudo halt")
