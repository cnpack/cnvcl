object frmGIFDemo: TfrmGIFDemo
  Left = 341
  Top = 168
  Width = 604
  Height = 452
  Caption = 'GIF 演示'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pbDisplay: TPaintBox
    Left = 200
    Top = 8
    Width = 372
    Height = 404
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = pbDisplayPaint
  end
  object lblFileName: TLabel
    Left = 12
    Top = 56
    Width = 177
    Height = 16
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblInfo: TLabel
    Left = 12
    Top = 80
    Width = 177
    Height = 16
    AutoSize = False
  end
  object lblFrame: TLabel
    Left = 12
    Top = 104
    Width = 177
    Height = 16
    AutoSize = False
  end
  object btnLoad: TButton
    Left = 12
    Top = 12
    Width = 177
    Height = 33
    Caption = '加载 GIF'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object btnPrevFrame: TButton
    Left = 12
    Top = 136
    Width = 80
    Height = 29
    Caption = '上一幅'
    Enabled = False
    TabOrder = 1
    OnClick = btnPrevFrameClick
  end
  object btnNextFrame: TButton
    Left = 109
    Top = 136
    Width = 80
    Height = 29
    Caption = '下一幅'
    Enabled = False
    TabOrder = 2
    OnClick = btnNextFrameClick
  end
  object btnPlayPause: TButton
    Left = 12
    Top = 180
    Width = 177
    Height = 33
    Caption = '播放'
    Enabled = False
    TabOrder = 3
    OnClick = btnPlayPauseClick
  end
  object dlgOpen: TOpenDialog
    Filter = 'GIF 文件(*.gif)|*.gif|所有文件(*.*)|*.*'
    Left = 72
    Top = 240
  end
  object tmrAnimation: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrAnimationTimer
    Left = 72
    Top = 296
  end
end
