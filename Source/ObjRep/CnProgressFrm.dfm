object ProgressForm: TProgressForm
  Left = 265
  Top = 265
  AutoSize = True
  BorderStyle = bsNone
  Caption = '进度显示'
  ClientHeight = 81
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 12
  object SpeedButton1: TSpeedButton
    Left = 0
    Top = 0
    Width = 297
    Height = 81
    Enabled = False
  end
  object lblTitle: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 12
    Caption = 'lblTitle'
  end
  object Label1: TLabel
    Left = 8
    Top = 27
    Width = 54
    Height = 12
    Caption = '请稍候...'
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 48
    Width = 281
    Height = 23
    Min = 0
    Max = 100
    Smooth = True
    TabOrder = 0
  end
end
