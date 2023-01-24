object Form1: TForm1
  Left = 208
  Top = 139
  Width = 565
  Height = 399
  Caption = 'CameraEye Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 449
    Height = 305
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = Button2Click
  end
  object btnVideoFormat: TButton
    Left = 472
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Video Format'
    TabOrder = 3
    OnClick = btnVideoFormatClick
  end
  object btnVideoSource: TButton
    Left = 472
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Video Source'
    TabOrder = 4
    OnClick = btnVideoSourceClick
  end
  object btnVideoDisplay: TButton
    Left = 472
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Video Display'
    TabOrder = 5
    OnClick = btnVideoDisplayClick
  end
  object btnVideoCompression: TButton
    Left = 472
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Video Compress'
    TabOrder = 6
    OnClick = btnVideoCompressionClick
  end
  object CnCameraEye1: TCnCameraEye
    Display = Panel1
    Left = 208
    Top = 208
  end
end
