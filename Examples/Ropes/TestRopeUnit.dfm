object TestRopeForm: TTestRopeForm
  Left = 198
  Top = 92
  Width = 904
  Height = 623
  Caption = 'Test Ropes String'
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
  object grpFastPos: TGroupBox
    Left = 16
    Top = 16
    Width = 857
    Height = 73
    Caption = 'Fast Position'
    TabOrder = 0
    object edtPattern: TEdit
      Left = 24
      Top = 28
      Width = 81
      Height = 21
      TabOrder = 1
      Text = 'sad'
    end
    object edtStr: TEdit
      Left = 120
      Top = 28
      Width = 609
      Height = 21
      TabOrder = 2
      Text = 'I am sad for the world.'
    end
    object btnSearch: TButton
      Left = 760
      Top = 24
      Width = 73
      Height = 25
      Caption = 'Search'
      TabOrder = 0
      OnClick = btnSearchClick
    end
  end
  object grpRopes: TGroupBox
    Left = 16
    Top = 112
    Width = 857
    Height = 449
    Caption = 'Ropes'
    TabOrder = 1
    object lbl1: TLabel
      Left = 344
      Top = 164
      Width = 13
      Height = 13
      Caption = 'To'
    end
    object mmoRope: TMemo
      Left = 24
      Top = 32
      Width = 233
      Height = 257
      Lines.Strings = (
        '  Test String for Rope.')
      TabOrder = 0
      OnChange = mmoRopeChange
    end
    object edtAppend: TEdit
      Left = 280
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'To Append'
    end
    object btnAppend: TButton
      Left = 424
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Append'
      TabOrder = 2
      OnClick = btnAppendClick
    end
    object mmoResult: TMemo
      Left = 24
      Top = 312
      Width = 809
      Height = 113
      ReadOnly = True
      TabOrder = 11
    end
    object btnTrim: TButton
      Left = 280
      Top = 72
      Width = 75
      Height = 25
      Caption = 'Trim'
      TabOrder = 5
      OnClick = btnTrimClick
    end
    object btnEqual: TButton
      Left = 512
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Equal?'
      TabOrder = 3
      OnClick = btnEqualClick
    end
    object btnReverse: TButton
      Left = 280
      Top = 112
      Width = 75
      Height = 25
      Caption = 'Reverse'
      TabOrder = 6
      OnClick = btnReverseClick
    end
    object udStart: TUpDown
      Left = 321
      Top = 160
      Width = 15
      Height = 21
      Associate = edtStart
      Min = 0
      Position = 4
      TabOrder = 8
      Wrap = False
    end
    object edtStart: TEdit
      Left = 280
      Top = 160
      Width = 41
      Height = 21
      TabOrder = 7
      Text = '4'
    end
    object btnDelete: TButton
      Left = 432
      Top = 160
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 9
      OnClick = btnDeleteClick
    end
    object btnSubStr: TButton
      Left = 520
      Top = 160
      Width = 75
      Height = 25
      Caption = 'SubStr'
      TabOrder = 10
      OnClick = btnSubStrClick
    end
    object btnPos: TButton
      Left = 600
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Position'
      TabOrder = 4
      OnClick = btnPosClick
    end
  end
  object udEnd: TUpDown
    Left = 425
    Top = 272
    Width = 15
    Height = 21
    Associate = edtEnd
    Min = 0
    Position = 9
    TabOrder = 3
    Wrap = False
  end
  object edtEnd: TEdit
    Left = 384
    Top = 272
    Width = 41
    Height = 21
    TabOrder = 2
    Text = '9'
  end
end
