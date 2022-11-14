object FormGB18030: TFormGB18030
  Left = 192
  Top = 110
  Width = 982
  Height = 563
  Caption = 'Unicode and GB18030'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnCodePointFromUtf161: TButton
    Left = 16
    Top = 16
    Width = 121
    Height = 25
    Caption = 'Utf16 CodePoint 1'
    TabOrder = 0
    OnClick = btnCodePointFromUtf161Click
  end
  object btnCodePointFromUtf162: TButton
    Left = 16
    Top = 56
    Width = 121
    Height = 25
    Caption = 'Utf16 CodePoint 2'
    TabOrder = 1
    OnClick = btnCodePointFromUtf162Click
  end
  object btnUtf16CharLength: TButton
    Left = 160
    Top = 16
    Width = 137
    Height = 25
    Caption = 'Utf16 Char Length'
    TabOrder = 2
    OnClick = btnUtf16CharLengthClick
  end
  object btnUtf8CharLength: TButton
    Left = 160
    Top = 56
    Width = 137
    Height = 25
    Caption = 'Utf8 Char Length'
    TabOrder = 3
    OnClick = btnUtf8CharLengthClick
  end
  object btnUtf8Encode: TButton
    Left = 320
    Top = 16
    Width = 121
    Height = 25
    Caption = 'Encode to Utf8-MB4'
    TabOrder = 4
    OnClick = btnUtf8EncodeClick
  end
  object btnCodePointUtf16: TButton
    Left = 16
    Top = 96
    Width = 121
    Height = 25
    Caption = 'CodePoint Utf16 1'
    TabOrder = 5
    OnClick = btnCodePointUtf16Click
  end
  object btnCodePointUtf162: TButton
    Left = 16
    Top = 136
    Width = 121
    Height = 25
    Caption = 'CodePoint Utf16 2'
    TabOrder = 6
    OnClick = btnCodePointUtf162Click
  end
  object btnUtf8Decode: TButton
    Left = 320
    Top = 56
    Width = 121
    Height = 25
    Caption = 'Decode from Utf8-MB4'
    TabOrder = 7
    OnClick = btnUtf8DecodeClick
  end
  object btnGenUtf16: TButton
    Left = 792
    Top = 16
    Width = 169
    Height = 25
    Caption = 'Generate Utf16 Table'
    TabOrder = 8
    OnClick = btnGenUtf16Click
  end
  object btnCodePointUtf163: TButton
    Left = 16
    Top = 176
    Width = 121
    Height = 25
    Caption = 'CodePoint Utf16 3'
    TabOrder = 9
    OnClick = btnCodePointUtf163Click
  end
  object btnGB18030ToUtf16: TButton
    Left = 464
    Top = 16
    Width = 121
    Height = 25
    Caption = 'GB18030 to/from Utf16'
    TabOrder = 10
    OnClick = btnGB18030ToUtf16Click
  end
  object btn18030CodePoint1: TButton
    Left = 600
    Top = 16
    Width = 121
    Height = 25
    Caption = 'GB18030 CodePoint 1'
    TabOrder = 11
    OnClick = btn18030CodePoint1Click
  end
  object btn18030CodePoint2: TButton
    Left = 600
    Top = 56
    Width = 121
    Height = 25
    Caption = 'GB18030 CodePoint 2'
    TabOrder = 12
    OnClick = btn18030CodePoint2Click
  end
  object btn18030CodePoint3: TButton
    Left = 600
    Top = 96
    Width = 121
    Height = 25
    Caption = 'GB18030 CodePoint 3'
    TabOrder = 13
    OnClick = btn18030CodePoint3Click
  end
  object btnCodePoint180301: TButton
    Left = 600
    Top = 136
    Width = 121
    Height = 25
    Caption = 'CodePoint 18030 1'
    TabOrder = 14
    OnClick = btnCodePoint180301Click
  end
  object btnCodePoint180302: TButton
    Left = 600
    Top = 176
    Width = 121
    Height = 25
    Caption = 'CodePoint 18030 2'
    TabOrder = 15
    OnClick = btnCodePoint180302Click
  end
  object btnCodePoint180303: TButton
    Left = 600
    Top = 216
    Width = 121
    Height = 25
    Caption = 'CodePoint 18030 3'
    TabOrder = 16
    OnClick = btnCodePoint180303Click
  end
  object btnMultiUtf16ToGB18130: TButton
    Left = 464
    Top = 56
    Width = 121
    Height = 25
    Caption = 'Utf16s To GB18130s'
    TabOrder = 17
    OnClick = btnMultiUtf16ToGB18130Click
  end
  object btnMultiGB18131ToUtf16: TButton
    Left = 464
    Top = 96
    Width = 121
    Height = 25
    Caption = 'GB18030s to Utf16s'
    TabOrder = 18
    OnClick = btnMultiGB18131ToUtf16Click
  end
  object btnGenGB18030Page: TButton
    Left = 792
    Top = 96
    Width = 169
    Height = 113
    Caption = 'Generate GB18030 Utf16  Table'
    TabOrder = 19
    OnClick = btnGenGB18030PageClick
  end
  object btnGenUtf16Page: TButton
    Left = 792
    Top = 56
    Width = 169
    Height = 25
    Caption = 'Generate Utf16 GB18030 Page'
    TabOrder = 20
    OnClick = btnGenUtf16PageClick
  end
  object chkIncludeCharValue: TCheckBox
    Left = 792
    Top = 224
    Width = 145
    Height = 17
    Caption = 'Include Char Value'
    Checked = True
    State = cbChecked
    TabOrder = 21
  end
  object btnGB18303CodePointToUtf16: TButton
    Left = 792
    Top = 320
    Width = 169
    Height = 25
    Caption = 'GB18303 CodePoint To Unicode'
    TabOrder = 22
    OnClick = btnGB18303CodePointToUtf16Click
  end
  object btnGenGB18030PagePartly: TButton
    Left = 792
    Top = 248
    Width = 169
    Height = 41
    Caption = 'Generate GB18030 Utf16 Inc'
    TabOrder = 23
    OnClick = btnGenGB18030PagePartlyClick
  end
  object btnGenGB18030UnicodeCompare: TButton
    Left = 792
    Top = 360
    Width = 169
    Height = 25
    Caption = 'Generate GB18030 Unicode'
    TabOrder = 24
    OnClick = btnGenGB18030UnicodeCompareClick
  end
  object dlgSave1: TSaveDialog
    FileName = 'CHARS.txt'
    Left = 928
    Top = 40
  end
end
