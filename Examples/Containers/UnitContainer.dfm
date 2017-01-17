object FormContainers: TFormContainers
  Left = 266
  Top = 150
  BorderStyle = bsDialog
  Caption = 'Test Containers'
  ClientHeight = 396
  ClientWidth = 837
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblCount: TLabel
    Left = 744
    Top = 112
    Width = 34
    Height = 13
    Caption = 'Count: '
  end
  object lblRingBuffer: TLabel
    Left = 40
    Top = 16
    Width = 56
    Height = 13
    Caption = 'Ring Buffer:'
  end
  object StringGrid: TStringGrid
    Left = 40
    Top = 40
    Width = 737
    Height = 41
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    TabOrder = 0
  end
  object btnPushFront: TButton
    Left = 40
    Top = 112
    Width = 97
    Height = 25
    Caption = 'Push to Front'
    TabOrder = 1
    OnClick = btnPushFrontClick
  end
  object btnPushBack: TButton
    Left = 344
    Top = 112
    Width = 91
    Height = 25
    Caption = 'Push to Back'
    TabOrder = 2
    OnClick = btnPushBackClick
  end
  object btnPopFront: TButton
    Left = 448
    Top = 112
    Width = 89
    Height = 25
    Caption = 'Pop from Front'
    TabOrder = 3
    OnClick = btnPopFrontClick
  end
  object btnPopBack: TButton
    Left = 152
    Top = 112
    Width = 97
    Height = 25
    Caption = 'Pop from Back'
    TabOrder = 4
    OnClick = btnPopBackClick
  end
end
