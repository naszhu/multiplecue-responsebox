from psychopy import visual, event, core, data, logging, misc, gui

def checkmonitor( win, set_resx, set_resy, set_framerate, fr_tol = 1, maxnomesures = 100, nomesframes = 5, fullscreen = True ):

    core.wait(1)

    resx = win.size[0]
    resy = win.size[1]

    TextStim1 = visual.TextStim(win, text = 'Measuring frame rate...', pos=[0,50], units='pix', autoLog=False)
    TextStim2 = visual.TextStim(win, text = '2', pos=[0,0], units='pix', autoLog=False)
    TextStim3 = visual.TextStim(win, text = '3', pos=[0,-50], units='pix', autoLog=False)

    TestClock = core.Clock()
    TestClock.reset()

    i = 0
    framerate = 0
    
    event.clearEvents()

    for j in range(10):
        win.flip()

    fr = []
    i = 0
    fcontinue = True

    while fcontinue:
        TextStim1.draw()
        TextStim2.draw()
        TextStim3.draw()

        win.flip()
        starttime = TestClock.getTime()

        for j in range(nomesframes):
            TextStim1.draw()
            TextStim2.draw()
            TextStim3.draw()
            win.flip()
#            core.wait(0.002)

        framerate = TestClock.getTime()-starttime

        fr.append( round(1/(framerate/nomesframes),1) )
        
        if resx !=set_resx or resy != set_resy:
            TextStim2.setColor('red')
        else:
            TextStim2.setColor('white')
            
        if fr[i] > set_framerate-fr_tol and  fr[i] < set_framerate+fr_tol:
            TextStim3.setColor('white')
        else:
            TextStim3.setColor('red')
        
        TextStim2.setText('Resolution = '+str(resx)+' x '+str(resy))
        TextStim3.setText('Frame rate = '+str(fr[i])+' Hz')
        i = i+1
        thisKey = event.getKeys()

        if( len(thisKey) > 0 ):
            if thisKey[0] == 'escape':
                fcontinue = False
        
        if( i > maxnomesures ):
            fcontinue = False

#    print(fr)
    
    fr = fr[10:]

    tfr = []

    for i in fr:
        tfr.append( i > set_framerate-fr_tol and  i < set_framerate+fr_tol )

#    print(tfr)

    itfr = list(map(int, tfr))

    frameslips = len(itfr) - sum(itfr)

#    win.close()

    # Hide the window away rather than closing it. Then restore it with:
    # win.winHandle.minimize() # minimise the PsychoPy window
    if fullscreen:
        win.winHandle.set_fullscreen(False) # disable fullscreen
        win.flip() # redraw the (minimised) window

    if resx !=set_resx or resy != set_resy:
        myDlg = gui.Dlg(title="Error!")
        myDlg.addText('Screen resolution not '+str(set_resx)+'x'+str(set_resy)+'!!')
        myDlg.addText('\nPress OK to continue nonetheless.')
        outDlg = myDlg.show() #show dialog and wait for OK or Cancel
        if not myDlg.OK:
            core.quit()

    if frameslips > 0:
        myDlg = gui.Dlg(title="Error!")
        myDlg.addText('Refresh rate out of range in '+str(frameslips)+' cases!!')
        myDlg.addText('\nPress OK to continue nonetheless.')
        outDlg = myDlg.show() #show dialog and wait for OK or Cancel
        if not myDlg.OK:
            core.quit()

    # Unhide window
    if fullscreen:
        win.winHandle.maximize()
        win.winHandle.set_fullscreen(True) 
        win.winHandle.activate()
        win.flip()

    return(fr)




