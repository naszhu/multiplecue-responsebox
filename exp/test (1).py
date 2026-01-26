import serial
import time
from psychopy import visual, core, event

# 1. 初始化串口 [cite: 133, 190]
try:
    ser = serial.Serial('/dev/ttyACM0', 115200, timeout=1)
    core.wait(2.0) # 等待串口稳定 [cite: 724]
except Exception as e:
    print(f"串口连接失败: {e}")
    core.quit()

win = visual.Window([800, 600], fullscr=False, color="black", units="pix")
fixation = visual.TextStim(win, text="+", color="white")
target = visual.Circle(win, radius=50, fillColor="red", lineColor="white")
feedback = visual.TextStim(win, text="", pos=(0, -100))

def run_trial():
    fixation.draw()
    win.flip()
    core.wait(1.0 + (time.time() % 1)) # 随机等待

    target.draw()
    ser.reset_input_buffer() # 呈现前清空缓存，消除误触 [cite: 252]
    win.flip() 
    
    ser.write(b'S') # 发送同步信号给 Arduino [cite: 151, 297]
    
    rt_ms = None
    btn_id = None
    start_watch = core.getTime()
    
    while (core.getTime() - start_watch < 3.0): # 3秒超时
        if ser.in_waiting > 0:
            # 读取并拆分数据 (格式: "按键,时间") [cite: 254, 284]
            raw_data = ser.readline().decode().strip()
            if ',' in raw_data:
                parts = raw_data.split(',')
                btn_id = parts[0]
                rt_ms = int(parts[1]) / 1000.0 # 微秒转毫秒 [cite: 290, 646]
                break
        
        if 'escape' in event.getKeys():
            core.quit()

    return btn_id, rt_ms

# 3. 运行测试
for i in range(5):
    btn, rt = run_trial()
    if rt:
        feedback.text = f"按键: {btn} | RT: {rt:.2f} ms"
    else:
        feedback.text = "超时未响应"
    
    feedback.draw()
    win.flip()
    core.wait(1.0)

win.close()
ser.close()
core.quit()


core.quit(())