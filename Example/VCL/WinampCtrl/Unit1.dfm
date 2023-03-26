object Form1: TForm1
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Test for TCnWinampCtrl'
  ClientHeight = 385
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'ËÎÌו'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object lbl1: TLabel
    Left = 8
    Top = 232
    Width = 54
    Height = 12
    Caption = 'PlayList:'
  end
  object lblTime: TLabel
    Left = 8
    Top = 123
    Width = 42
    Height = 12
    Caption = 'lblTime'
  end
  object lblLength: TLabel
    Left = 96
    Top = 123
    Width = 54
    Height = 12
    Caption = 'lblLength'
  end
  object lblVolume: TLabel
    Left = 360
    Top = 123
    Width = 42
    Height = 12
    Caption = 'Volume:'
  end
  object lblTitle: TLabel
    Left = 8
    Top = 88
    Width = 48
    Height = 12
    Caption = 'lblTitle'
  end
  object lblPath: TLabel
    Left = 168
    Top = 84
    Width = 72
    Height = 12
    Caption = 'Winamp Path:'
  end
  object btnShufleOn: TButton
    Left = 168
    Top = 56
    Width = 75
    Height = 21
    Caption = 'Shufle On'
    TabOrder = 12
    OnClick = btnShufleOnClick
  end
  object btnShufleOff: TButton
    Left = 248
    Top = 56
    Width = 75
    Height = 21
    Caption = 'Shufle Off'
    TabOrder = 13
    OnClick = btnShufleOffClick
  end
  object btnGetPlayList: TButton
    Left = 328
    Top = 56
    Width = 75
    Height = 21
    Caption = 'GetPlayList'
    TabOrder = 14
    OnClick = btnGetPlayListClick
  end
  object lst1: TListBox
    Left = 8
    Top = 248
    Width = 393
    Height = 129
    ItemHeight = 12
    TabOrder = 36
    OnDblClick = lst1DblClick
  end
  object btnFindWinamp: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 21
    Caption = 'FindWinamp'
    TabOrder = 0
    OnClick = btnFindWinampClick
  end
  object btnPrevTrack: TButton
    Left = 8
    Top = 32
    Width = 75
    Height = 21
    Caption = 'PrevTrack'
    TabOrder = 5
    OnClick = btnPrevTrackClick
  end
  object btnPlay: TButton
    Left = 88
    Top = 32
    Width = 75
    Height = 21
    Caption = 'Play'
    TabOrder = 6
    OnClick = btnPlayClick
  end
  object btnPause: TButton
    Left = 168
    Top = 32
    Width = 75
    Height = 21
    Caption = 'Pause'
    TabOrder = 7
    OnClick = btnPauseClick
  end
  object btnStop: TButton
    Left = 248
    Top = 32
    Width = 75
    Height = 21
    Caption = 'Stop'
    TabOrder = 8
    OnClick = btnStopClick
  end
  object btnNextTrack: TButton
    Left = 328
    Top = 32
    Width = 75
    Height = 21
    Caption = 'NextTrack'
    TabOrder = 9
    OnClick = btnNextTrackClick
  end
  object btnGetVersion: TButton
    Left = 328
    Top = 8
    Width = 75
    Height = 21
    Caption = 'GetVersion'
    TabOrder = 4
    OnClick = btnGetVersionClick
  end
  object btnRepeatOn: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 21
    Caption = 'Repeat On'
    TabOrder = 10
    OnClick = btnRepeatOnClick
  end
  object btnRepeatOff: TButton
    Left = 88
    Top = 56
    Width = 75
    Height = 21
    Caption = 'Repeat Off'
    TabOrder = 11
    OnClick = btnRepeatOffClick
  end
  object btnIsFound: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 21
    Caption = 'IsFound'
    TabOrder = 1
    OnClick = btnIsFoundClick
  end
  object tbTime: TTrackBar
    Left = 8
    Top = 104
    Width = 393
    Height = 17
    Ctl3D = True
    Max = 0
    Orientation = trHorizontal
    ParentCtl3D = False
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 15
    ThumbLength = 10
    TickMarks = tmBoth
    TickStyle = tsNone
  end
  object btnVolumeUp: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 21
    Caption = 'VolumeUp'
    TabOrder = 2
    OnClick = btnVolumeUpClick
  end
  object btnVolumeDown: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 21
    Caption = 'VolumeDown'
    TabOrder = 3
    OnClick = btnVolumeDownClick
  end
  object tbVolume: TTrackBar
    Left = 368
    Top = 160
    Width = 25
    Height = 73
    Max = 0
    Min = -255
    Orientation = trVertical
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 32
    TabStop = False
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbVolumeChange
  end
  object tbVolBalance: TTrackBar
    Left = 360
    Top = 136
    Width = 38
    Height = 17
    Max = 127
    Min = -127
    Orientation = trHorizontal
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 20
    ThumbLength = 10
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbVolBalanceChange
  end
  object btnStartWinamp: TButton
    Left = 8
    Top = 136
    Width = 75
    Height = 21
    Caption = 'StartWinamp'
    TabOrder = 16
    OnClick = btnStartWinampClick
  end
  object btnCloseWinamp: TButton
    Left = 88
    Top = 136
    Width = 75
    Height = 21
    Caption = 'CloseWinamp'
    TabOrder = 17
    OnClick = btnCloseWinampClick
  end
  object btnEnabledWAWindow: TButton
    Left = 168
    Top = 136
    Width = 75
    Height = 21
    Caption = 'EnabledWA'
    TabOrder = 18
    OnClick = btnEnabledWAWindowClick
  end
  object btnDisabledWAWindow: TButton
    Left = 248
    Top = 136
    Width = 75
    Height = 21
    Caption = 'DisabledWA'
    TabOrder = 19
    OnClick = btnDisabledWAWindowClick
  end
  object tbEQ10: TTrackBar
    Left = 8
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 21
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ10Change
  end
  object tbEQ0: TTrackBar
    Left = 48
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 22
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ0Change
  end
  object tbEQ1: TTrackBar
    Left = 72
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 23
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ1Change
  end
  object tbEQ2: TTrackBar
    Left = 96
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 24
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ2Change
  end
  object tbEQ3: TTrackBar
    Left = 120
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 25
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ3Change
  end
  object tbEQ4: TTrackBar
    Left = 144
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 26
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ4Change
  end
  object tbEQ5: TTrackBar
    Left = 168
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 27
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ5Change
  end
  object tbEQ6: TTrackBar
    Left = 192
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 28
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ6Change
  end
  object tbEQ7: TTrackBar
    Left = 216
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 29
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ7Change
  end
  object tbEQ8: TTrackBar
    Left = 240
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 30
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ8Change
  end
  object tbEQ9: TTrackBar
    Left = 264
    Top = 160
    Width = 17
    Height = 73
    Max = 63
    Orientation = trVertical
    Frequency = 1
    Position = 32
    SelEnd = 0
    SelStart = 0
    TabOrder = 31
    ThumbLength = 15
    TickMarks = tmBoth
    TickStyle = tsNone
    OnChange = tbEQ9Change
  end
  object btnEnabledEQ: TButton
    Left = 288
    Top = 168
    Width = 75
    Height = 21
    Caption = 'EQ On'
    TabOrder = 33
    OnClick = btnEnabledEQClick
  end
  object btnEQAutoLoad: TButton
    Left = 288
    Top = 192
    Width = 75
    Height = 21
    Caption = 'EQ Load Off'
    TabOrder = 34
    OnClick = btnEQAutoLoadClick
  end
  object btnAbout: TButton
    Left = 288
    Top = 216
    Width = 75
    Height = 21
    Caption = 'About'
    TabOrder = 35
    OnClick = btnAboutClick
  end
  object edtPath: TEdit
    Left = 248
    Top = 80
    Width = 153
    Height = 20
    TabOrder = 37
    Text = 'C:\Program Files\Winamp\Winamp.exe'
  end
  object CnTimerList1: TCnTimerList
    Items = <
      item
        Enabled = False
        Interval = 1
        OnTimer = CnTimerList1Items0Timer
      end
      item
        Enabled = False
        Interval = 500
        OnTimer = CnTimerList1Items1Timer
      end>
    Left = 376
    Top = 352
  end
  object CtrlWinamp: TCnWinampCtrl
    AutoFind = True
    Left = 344
    Top = 352
  end
end
