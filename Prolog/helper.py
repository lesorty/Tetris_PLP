import keyboard
import time
import threading
import os

# when any key that is not esc is pressed, write '.' and enter.
#when esc is pressed, stop the program.
def write_dot(keyName):
    for _ in range(0, 3):
        keyboard.press_and_release('backspace')
    keyboard.write(keyName + '.')
    keyboard.press_and_release('enter')

allowedKeys = 'wasdcvxjkl'
commandKeys = ['backspace', 'enter', 'space', 'esc', 'tab', 'shift', 'ctrl', 'alt', 'caps lock', 'num lock', 'scroll lock', 'insert', 'home', 'page up', 'page down', 'end', 'right', 'left', 'down', 'up', 'print screen', 'pause', 'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9', 'f11', 'f12']

autoEnter = False
stop = False
writing = False
lastPress = time.time()

def catchAndPrint():
    while True:
        global stop, writing, lastPress
        event = keyboard.read_event()
        if event.name == 'esc':
            stop = True
            keyboard.write('x.')
            keyboard.press_and_release('enter')
            break
        if event.event_type == 'up': continue
        if writing: 
            keyboard.press_and_release('backspace')
            continue
        if event.name == 'p':
            global autoEnter
            autoEnter = not autoEnter
            keyboard.press_and_release('backspace')
        elif event.name in allowedKeys:
            writing = True
            lastPress = time.time()
            write_dot(event.name)
            writing = False
        elif event.name == 'm':
            keyboard.write('ain.')
            keyboard.press_and_release('enter')
        elif event.name not in commandKeys:
            keyboard.press_and_release('backspace')
    
def refresh():
    while True:
        global stop, writing
        if stop: break
        time.sleep(0.15)
        global autoEnter
        if autoEnter and not writing and time.time() - lastPress > 0.2:
            writing = True
            keyboard.write('n.') 
            keyboard.press_and_release('enter')
            writing = False

def main():
    catchAndPrintThread = threading.Thread(target=catchAndPrint)
    refreshThread = threading.Thread(target=refresh)
    catchAndPrintThread.start()
    refreshThread.start()

main()
