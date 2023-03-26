object Form1: TForm1
  Left = 518
  Top = 338
  Width = 437
  Height = 331
  Caption = 'Test for TCnVolumeCtrl'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'ËÎÌו'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object lbl1: TLabel
    Left = 228
    Top = 12
    Width = 42
    Height = 12
    Caption = 'Device:'
  end
  object lbl2: TLabel
    Left = 244
    Top = 40
    Width = 30
    Height = 12
    Caption = 'Line:'
  end
  object mmo1: TMemo
    Left = 24
    Top = 8
    Width = 193
    Height = 225
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btn1: TButton
    Left = 24
    Top = 240
    Width = 95
    Height = 25
    Caption = 'Get All State'
    TabOrder = 1
    OnClick = btn1Click
  end
  object cbb1: TComboBox
    Left = 276
    Top = 8
    Width = 145
    Height = 20
    Style = csDropDownList
    ItemHeight = 12
    TabOrder = 2
    OnChange = cbb1Change
  end
  object cbb2: TComboBox
    Tag = 1
    Left = 276
    Top = 36
    Width = 145
    Height = 20
    Style = csDropDownList
    ItemHeight = 12
    TabOrder = 3
    OnChange = cbb1Change
  end
  object TrackBar1: TTrackBar
    Left = 264
    Top = 68
    Width = 150
    Height = 33
    Max = 32
    Min = -32
    Orientation = trHorizontal
    Frequency = 32
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 320
    Top = 100
    Width = 41
    Height = 169
    Max = 255
    Orientation = trVertical
    Frequency = 47
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 5
    TickMarks = tmBoth
    TickStyle = tsAuto
    OnChange = TrackBar2Change
  end
  object chk1: TCheckBox
    Left = 320
    Top = 264
    Width = 41
    Height = 17
    Caption = 'Mute'
    TabOrder = 6
    OnClick = chk1Click
  end
  object btn2: TButton
    Left = 123
    Top = 240
    Width = 95
    Height = 25
    Caption = 'About'
    TabOrder = 7
    OnClick = btn2Click
  end
  object CnVolumeCtrl1: TCnVolumeCtrl
    About = 'CnPack'
    CurDev = 0
    CurLine = 0
    Volume = 255
    Balance = 0
    IsMute = False
    OnVolumeChange = CnVolumeCtrl1VolumeChange
    OnMuteChange = CnVolumeCtrl1MuteChange
    Left = 228
    Top = 132
  end
end
