import smbus
import os

bus = smbus.SMBus(1)

# if we are on battery power
if bus.read_byte_data(0x69,0)==2:
    # "unconditional file safe shutdown and power OFF when battery powered"
    bus.write_byte_data(0x6b,0x00,0xcc)
    
    # shut down nicely
    #os.system("sudo halt")
