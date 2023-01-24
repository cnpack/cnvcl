object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'SystemDebugControl Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 152
    Top = 32
    Width = 47
    Height = 13
    Caption = 'Address $'
  end
  object lblLen: TLabel
    Left = 312
    Top = 32
    Width = 36
    Height = 13
    Caption = 'Length:'
  end
  object btnKey: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Key'
    TabOrder = 0
    OnClick = btnKeyClick
  end
  object Memo1: TMemo
    Left = 24
    Top = 80
    Width = 73
    Height = 353
    TabOrder = 1
  end
  object btnKernelMem: TButton
    Left = 464
    Top = 28
    Width = 89
    Height = 25
    Caption = 'Read Kernel'
    TabOrder = 2
    OnClick = btnKernelMemClick
  end
  object edtLen: TEdit
    Left = 352
    Top = 28
    Width = 73
    Height = 21
    TabOrder = 3
    Text = '100'
  end
  object UpDown1: TUpDown
    Left = 425
    Top = 28
    Width = 15
    Height = 21
    Associate = edtLen
    Min = 0
    Position = 100
    TabOrder = 4
    Wrap = False
  end
  object cbbMem: TComboBox
    Left = 216
    Top = 28
    Width = 81
    Height = 21
    ItemHeight = 13
    TabOrder = 5
    Text = '804d1000'
    Items.Strings = (
      '80100000'
      '80400000'
      '804d1000'
      '804e0000')
  end
  object mmoMem: TMemo
    Left = 152
    Top = 88
    Width = 505
    Height = 89
    TabOrder = 6
  end
  object btnReadPhy: TButton
    Left = 568
    Top = 28
    Width = 89
    Height = 25
    Caption = 'Read Physical'
    TabOrder = 7
    OnClick = btnReadPhyClick
  end
  object btnChangekernelBase: TButton
    Left = 152
    Top = 200
    Width = 505
    Height = 25
    Caption = 'Change the '#39'MZ'#39' at Kernel Base to '#39'CH'#39
    TabOrder = 8
    OnClick = btnChangekernelBaseClick
  end
  object btnBeep: TButton
    Left = 152
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Start Beep'
    TabOrder = 9
    OnClick = btnBeepClick
  end
  object btnStopBeep: TButton
    Left = 256
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Stop Beep'
    TabOrder = 10
    OnClick = btnStopBeepClick
  end
  object btnCmos: TButton
    Left = 584
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Read CMOS'
    TabOrder = 11
    OnClick = btnCmosClick
  end
  object btnReadFirstHardDiskSn: TButton
    Left = 416
    Top = 248
    Width = 147
    Height = 25
    Caption = 'Read First HardDisk SN'
    TabOrder = 12
    OnClick = btnReadFirstHardDiskSnClick
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 48
    Top = 104
  end
end
