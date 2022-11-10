object FormGB18030: TFormGB18030
  Left = 192
  Top = 107
  Width = 979
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
end
