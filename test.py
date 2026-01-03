import serial
import time
from psychopy import visual, core, event
# from psychopy import visual, monitors



# 1. 初始化串口 [cite: 133, 190]
# 确保端口号与你 Ubuntu 识别的一致 (/dev/ttyACM0)
try:
    ser = serial.Serial('/dev/ttyACM0', 115200, timeout=1)
    # 给 Arduino 2秒的重启/稳定时间，这是论文中提到的安全期 [cite: 724]
    core.wait(2.0) 
except Exception as e:
    print(f"串口连接失败: {e}")
    core.quit()

# 2. 设置实验窗口和刺激
# 把 units="deg" 改为 units="pix"
win = visual.Window([640, 480], fullscr=False, color="black", units="pix")
fixation = visual.TextStim(win, text="+", color="white")
target = visual.Circle(win, radius=2, fillColor="red", lineColor="white")
feedback = visual.TextStim(win, text="", pos=(0, -5))

def run_trial(trial_num):
    # 显示注视点，随机等待 1-2 秒
    fixation.draw()
    win.flip()
    core.wait(1.5 + (time.time() % 1))

    # --- 关键步骤 A: 准备呈现刺激 ---
    target.draw()
    
    # 清空串口旧数据，防止之前的误触干扰 [cite: 252]
    ser.reset_input_buffer()
    
    # 刷屏瞬间记录视觉起始点 
    # win.flip() 是图像真正出现在显示器上的时刻
    win.flip() 
    
    # --- 关键步骤 B: 发送同步指令 'S' ---
    # 根据论文，向 Arduino 发送信号开启内部计时 [cite: 151, 297]
    ser.write(b'S') 
    
    # --- 关键步骤 C: 监听 Arduino 回传的 RT ---
    rt_ms = None
    response_received = False
    start_watch = core.getTime()
    
    while not response_received and (core.getTime() - start_watch < 3.0): # 3秒超时
        if ser.in_waiting > 0:
            # 读取 Arduino 传回的一行数据（单位为微秒）[cite: 154, 304]
            raw_data = ser.readline().decode().strip()
            try:
                rt_us = int(raw_data)
                rt_ms = rt_us / 1000.0  # 换算成毫秒以符合实验标准 [cite: 290, 646]
                response_received = True
            except ValueError:
                continue
        
        # 允许按 Esc 退出
        if 'escape' in event.getKeys():
            win.close()
            ser.close()
            core.quit()

    return rt_ms

# 3. 运行 5 个测试 Trial
results = []
for i in range(5):
    rt = run_trial(i + 1)
    if rt:
        results.append(rt)
        feedback.text = f"Trial {i+1} RT: {rt:.2f} ms"
    else:
        feedback.text = "未检测到按键 (Timeout)"
    
    feedback.draw()
    win.flip()
    core.wait(1.0)

# 打印最终统计数据 [cite: 54, 78]
if results:
    avg_rt = sum(results) / len(results)
    print(f"\n实验结束！平均反应时: {avg_rt:.2f} ms")

win.close()
ser.close()
core.quit()