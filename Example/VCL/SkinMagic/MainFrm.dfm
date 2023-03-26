object MainForm: TMainForm
  Left = 330
  Top = 193
  Width = 580
  Height = 481
  Caption = 'CnSkinMagic Demo by savetime, CnPack Team'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 572
    Height = 49
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 105
      Height = 25
      Caption = '&Enable Skin'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 120
      Top = 8
      Width = 105
      Height = 25
      Caption = '&Disable Skin'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 232
      Top = 8
      Width = 105
      Height = 25
      Caption = '&Runtime Create'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button5: TButton
      Left = 456
      Top = 8
      Width = 105
      Height = 25
      Caption = 'CnPack'
      TabOrder = 3
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 344
      Top = 8
      Width = 105
      Height = 25
      Caption = '&New Instance'
      TabOrder = 4
      OnClick = Button6Click
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 56
    Width = 241
    Height = 97
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 336
    Width = 553
    Height = 105
    Caption = 'DB Controls'
    TabOrder = 2
    object DBEdit1: TDBEdit
      Left = 8
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object DBMemo1: TDBMemo
      Left = 136
      Top = 24
      Width = 409
      Height = 73
      TabOrder = 1
    end
    object DBEdit2: TDBEdit
      Left = 8
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 2
    end
    object DBEdit3: TDBEdit
      Left = 8
      Top = 72
      Width = 121
      Height = 21
      TabOrder = 3
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 160
    Width = 553
    Height = 169
    Caption = 'TWinControl'
    TabOrder = 3
    object RadioButton1: TRadioButton
      Left = 8
      Top = 24
      Width = 113
      Height = 17
      Caption = 'RadioButton1'
      TabOrder = 0
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 48
      Width = 113
      Height = 17
      Caption = 'RadioButton2'
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 8
      Top = 72
      Width = 113
      Height = 17
      Caption = 'RadioButton3'
      TabOrder = 2
    end
    object CheckBox1: TCheckBox
      Left = 136
      Top = 24
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 3
    end
    object CheckBox2: TCheckBox
      Left = 136
      Top = 48
      Width = 97
      Height = 17
      Caption = 'CheckBox2'
      TabOrder = 4
    end
    object CheckBox3: TCheckBox
      Left = 136
      Top = 72
      Width = 97
      Height = 17
      Caption = 'CheckBox3'
      TabOrder = 5
    end
    object RadioGroup1: TRadioGroup
      Left = 264
      Top = 16
      Width = 129
      Height = 73
      Caption = 'RadioGroup1'
      Items.Strings = (
        'Items'
        'Item2'
        'Item3')
      TabOrder = 6
    end
    object Memo1: TMemo
      Left = 136
      Top = 96
      Width = 105
      Height = 65
      Lines.Strings = (
        'Memo1')
      ScrollBars = ssVertical
      TabOrder = 7
    end
    object ListBox1: TListBox
      Left = 8
      Top = 96
      Width = 105
      Height = 65
      ItemHeight = 13
      Items.Strings = (
        'List1'
        'List2'
        'List3')
      TabOrder = 8
    end
    object ComboBox1: TComboBox
      Left = 264
      Top = 100
      Width = 129
      Height = 21
      ItemHeight = 13
      TabOrder = 9
      Text = 'ComboBox1'
    end
    object Edit1: TEdit
      Left = 416
      Top = 20
      Width = 121
      Height = 21
      TabOrder = 10
      Text = 'Edit1'
    end
    object Edit2: TEdit
      Left = 416
      Top = 44
      Width = 121
      Height = 21
      TabOrder = 11
      Text = 'Edit2'
    end
    object Edit3: TEdit
      Left = 416
      Top = 68
      Width = 121
      Height = 21
      TabOrder = 12
      Text = 'Edit3'
    end
    object MaskEdit1: TMaskEdit
      Left = 264
      Top = 124
      Width = 129
      Height = 21
      TabOrder = 13
      Text = 'MaskEdit1'
    end
    object Button4: TButton
      Left = 416
      Top = 128
      Width = 121
      Height = 25
      Caption = 'Button4'
      TabOrder = 14
    end
    object BitBtn1: TBitBtn
      Left = 416
      Top = 96
      Width = 121
      Height = 25
      Caption = 'BitBtn1'
      TabOrder = 15
    end
  end
  object GroupBox3: TGroupBox
    Left = 256
    Top = 56
    Width = 305
    Height = 97
    Caption = 'TControl'
    TabOrder = 4
    object SpeedButton1: TSpeedButton
      Left = 8
      Top = 24
      Width = 97
      Height = 25
    end
    object Bevel1: TBevel
      Left = 120
      Top = 24
      Width = 50
      Height = 50
    end
    object Bevel2: TBevel
      Left = 184
      Top = 24
      Width = 50
      Height = 50
    end
    object SpeedButton2: TSpeedButton
      Left = 8
      Top = 56
      Width = 97
      Height = 25
    end
    object Shape1: TShape
      Left = 248
      Top = 24
      Width = 49
      Height = 49
    end
  end
  object CnSkinMagic1: TCnSkinMagic
    Left = 184
    Top = 96
  end
end
