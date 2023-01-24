object Form1: TForm1
  Left = 242
  Top = 109
  Width = 590
  Height = 448
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 421
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Edit1: TEdit
      Left = 8
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 72
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 1
    end
    object Button1: TButton
      Left = 8
      Top = 104
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 2
    end
    object BitBtn1: TBitBtn
      Left = 8
      Top = 320
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Apply'
      TabOrder = 3
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 8
      Top = 352
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Reset'
      TabOrder = 4
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 6
      Top = 384
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Default'
      TabOrder = 5
      OnClick = BitBtn3Click
    end
    object HotKey1: THotKey
      Left = 8
      Top = 144
      Width = 121
      Height = 19
      HotKey = 32833
      InvalidKeys = [hcNone, hcShift]
      Modifiers = [hkAlt]
      TabOrder = 6
    end
    object Memo1: TMemo
      Left = 8
      Top = 176
      Width = 121
      Height = 113
      Lines.Strings = (
        'Memo1'
        ''
        'This is a multi-line '
        'control.'
        ''
        'Test1.'
        'Test2.')
      TabOrder = 7
    end
  end
  object pnlTreeView: TPanel
    Left = 145
    Top = 0
    Width = 437
    Height = 421
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
