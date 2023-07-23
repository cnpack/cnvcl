object FormGB18030: TFormGB18030
  Left = 147
  Top = 93
  Width = 1162
  Height = 633
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
  object grpTestGB2U: TGroupBox
    Left = 240
    Top = 304
    Width = 337
    Height = 153
    Caption = 'Test GB18030 to Unicode'
    TabOrder = 2
    object btnGB18030CodePointToUtf16: TButton
      Left = 16
      Top = 24
      Width = 297
      Height = 25
      Caption = 'GB18030 CodePoint To Unicode'
      TabOrder = 0
      OnClick = btnGB18030CodePointToUtf16Click
    end
    object btnGenGB18030UnicodeCompare: TButton
      Left = 16
      Top = 64
      Width = 297
      Height = 25
      Caption = 'Generate GB18030 To Unicode to Compare to Table'
      TabOrder = 1
      OnClick = btnGenGB18030UnicodeCompareClick
    end
    object btnStringGB18030ToUnicode: TButton
      Left = 16
      Top = 104
      Width = 297
      Height = 25
      Caption = 'String GB18030 To Unicode'
      TabOrder = 2
      OnClick = btnStringGB18030ToUnicodeClick
    end
  end
  object grpGenerate: TGroupBox
    Left = 16
    Top = 16
    Width = 209
    Height = 569
    Caption = 'Generate Maps using API'
    TabOrder = 0
    object bvl1: TBevel
      Left = 16
      Top = 96
      Width = 169
      Height = 9
      Shape = bsTopLine
    end
    object bvl2: TBevel
      Left = 16
      Top = 432
      Width = 169
      Height = 17
      Shape = bsTopLine
    end
    object btnGenUtf16: TButton
      Left = 16
      Top = 16
      Width = 169
      Height = 25
      Caption = 'Generate Utf16 Table'
      TabOrder = 0
      OnClick = btnGenUtf16Click
    end
    object btnGenGB18030Page: TButton
      Left = 16
      Top = 112
      Width = 169
      Height = 113
      Caption = 'Generate GB18030 Utf16 Table'
      TabOrder = 2
      OnClick = btnGenGB18030PageClick
    end
    object btnGenUtf16Page: TButton
      Left = 16
      Top = 296
      Width = 169
      Height = 113
      Caption = 'Generate Utf16 GB18030 Table'
      TabOrder = 1
      OnClick = btnGenUtf16PageClick
    end
    object chkIncludeCharValue: TCheckBox
      Left = 32
      Top = 264
      Width = 145
      Height = 17
      Caption = 'Include Char Value'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object btnGenGB18030PagePartly: TButton
      Left = 16
      Top = 504
      Width = 169
      Height = 41
      Caption = 'Generate GB18030 Utf16 2 Inc'
      TabOrder = 5
      OnClick = btnGenGB18030PagePartlyClick
    end
    object btnGenUtf16Page1: TButton
      Left = 16
      Top = 448
      Width = 169
      Height = 25
      Caption = 'Generate Utf16 GB18030 Table'
      TabOrder = 4
      OnClick = btnGenUtf16Page1Click
    end
    object chkIncludeValue: TCheckBox
      Left = 32
      Top = 240
      Width = 145
      Height = 17
      Caption = 'Include Value'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object grpMisc: TGroupBox
    Left = 240
    Top = 16
    Width = 897
    Height = 265
    Caption = 'Misc'
    TabOrder = 1
    object btnCodePointFromUtf161: TButton
      Left = 16
      Top = 24
      Width = 121
      Height = 25
      Caption = 'Utf16 CodePoint 1'
      TabOrder = 0
      OnClick = btnCodePointFromUtf161Click
    end
    object btnCodePointFromUtf162: TButton
      Left = 16
      Top = 64
      Width = 121
      Height = 25
      Caption = 'Utf16 CodePoint 2'
      TabOrder = 5
      OnClick = btnCodePointFromUtf162Click
    end
    object btnUtf16CharLength: TButton
      Left = 160
      Top = 24
      Width = 137
      Height = 25
      Caption = 'Utf16 Char Length'
      TabOrder = 1
      OnClick = btnUtf16CharLengthClick
    end
    object btnUtf8CharLength: TButton
      Left = 160
      Top = 64
      Width = 137
      Height = 25
      Caption = 'Utf8 Char Length'
      TabOrder = 6
      OnClick = btnUtf8CharLengthClick
    end
    object btnUtf8Encode: TButton
      Left = 320
      Top = 24
      Width = 121
      Height = 25
      Caption = 'Encode to Utf8-MB4'
      TabOrder = 2
      OnClick = btnUtf8EncodeClick
    end
    object btnCodePointUtf16: TButton
      Left = 16
      Top = 104
      Width = 121
      Height = 25
      Caption = 'CodePoint Utf16 1'
      TabOrder = 10
      OnClick = btnCodePointUtf16Click
    end
    object btnCodePointUtf162: TButton
      Left = 16
      Top = 144
      Width = 121
      Height = 25
      Caption = 'CodePoint Utf16 2'
      TabOrder = 13
      OnClick = btnCodePointUtf162Click
    end
    object btnUtf8Decode: TButton
      Left = 320
      Top = 64
      Width = 121
      Height = 25
      Caption = 'Decode from Utf8-MB4'
      TabOrder = 7
      OnClick = btnUtf8DecodeClick
    end
    object btnCodePointUtf163: TButton
      Left = 16
      Top = 184
      Width = 121
      Height = 25
      Caption = 'CodePoint Utf16 3'
      TabOrder = 15
      OnClick = btnCodePointUtf163Click
    end
    object btnGB18030ToUtf16: TButton
      Left = 464
      Top = 24
      Width = 121
      Height = 25
      Caption = 'GB18030 to/from Utf16'
      TabOrder = 3
      OnClick = btnGB18030ToUtf16Click
    end
    object btn18030CodePoint1: TButton
      Left = 600
      Top = 24
      Width = 121
      Height = 25
      Caption = 'GB18030 CodePoint 1'
      TabOrder = 4
      OnClick = btn18030CodePoint1Click
    end
    object btn18030CodePoint2: TButton
      Left = 600
      Top = 64
      Width = 121
      Height = 25
      Caption = 'GB18030 CodePoint 2'
      TabOrder = 9
      OnClick = btn18030CodePoint2Click
    end
    object btn18030CodePoint3: TButton
      Left = 600
      Top = 104
      Width = 121
      Height = 25
      Caption = 'GB18030 CodePoint 3'
      TabOrder = 12
      OnClick = btn18030CodePoint3Click
    end
    object btnCodePoint180301: TButton
      Left = 600
      Top = 144
      Width = 121
      Height = 25
      Caption = 'CodePoint 18030 1'
      TabOrder = 14
      OnClick = btnCodePoint180301Click
    end
    object btnCodePoint180302: TButton
      Left = 600
      Top = 184
      Width = 121
      Height = 25
      Caption = 'CodePoint 18030 2'
      TabOrder = 16
      OnClick = btnCodePoint180302Click
    end
    object btnCodePoint180303: TButton
      Left = 600
      Top = 224
      Width = 121
      Height = 25
      Caption = 'CodePoint 18030 3'
      TabOrder = 17
      OnClick = btnCodePoint180303Click
    end
    object btnMultiUtf16ToGB18030: TButton
      Left = 464
      Top = 64
      Width = 121
      Height = 25
      Caption = 'Utf16s To GB18030s'
      TabOrder = 8
      OnClick = btnMultiUtf16ToGB18030Click
    end
    object btnMultiGB18030ToUtf16: TButton
      Left = 464
      Top = 104
      Width = 121
      Height = 25
      Caption = 'GB18030s to Utf16s'
      TabOrder = 11
      OnClick = btnMultiGB18030ToUtf16Click
    end
    object btnUnicodeIsDup: TButton
      Left = 16
      Top = 224
      Width = 121
      Height = 25
      Caption = 'Unicode Is Duplicate'
      TabOrder = 18
      OnClick = btnUnicodeIsDupClick
    end
    object btnCompareUnicodeString: TButton
      Left = 320
      Top = 192
      Width = 121
      Height = 25
      Caption = 'Compare Unicode 2'
      TabOrder = 19
      OnClick = btnCompareUnicodeStringClick
    end
    object btnCompareUnicodeString2: TButton
      Left = 320
      Top = 224
      Width = 121
      Height = 25
      Caption = 'Compare Unicode 3'
      TabOrder = 20
      OnClick = btnCompareUnicodeString2Click
    end
    object btnPinYinTest: TButton
      Left = 320
      Top = 144
      Width = 121
      Height = 25
      Caption = 'PinYin Test'
      TabOrder = 21
      OnClick = btnPinYinTestClick
    end
  end
  object grpTestU2GB: TGroupBox
    Left = 592
    Top = 304
    Width = 337
    Height = 153
    Caption = 'Test Unicode to GB18030 '
    TabOrder = 3
    object btnUtf16CodePointToGB180301: TButton
      Left = 16
      Top = 24
      Width = 297
      Height = 25
      Caption = 'Unicode CodePoint To GB18030'
      TabOrder = 0
      OnClick = btnUtf16CodePointToGB180301Click
    end
    object btnGenUnicodeGB18030Compare2: TButton
      Left = 16
      Top = 64
      Width = 297
      Height = 25
      Caption = 'Generate Unicode To GB18030 To Compare to Table'
      TabOrder = 1
      OnClick = btnGenUnicodeGB18030Compare2Click
    end
    object btnStringUnicodeToGB18030: TButton
      Left = 16
      Top = 104
      Width = 297
      Height = 25
      Caption = 'String Unicode To GB18030 '
      TabOrder = 2
      OnClick = btnStringUnicodeToGB18030Click
    end
  end
  object grpSequence: TGroupBox
    Left = 240
    Top = 472
    Width = 337
    Height = 113
    Caption = 'Sequence'
    TabOrder = 4
    object btnGenGB18030From0: TButton
      Left = 16
      Top = 24
      Width = 305
      Height = 25
      Caption = 'Generate GB18030 Chars From 0'
      TabOrder = 0
      OnClick = btnGenGB18030From0Click
    end
    object btnGenGB18030DownTo0: TButton
      Left = 16
      Top = 64
      Width = 305
      Height = 25
      Caption = 'Generate GB18030 Chars Downto 0'
      TabOrder = 1
      OnClick = btnGenGB18030DownTo0Click
    end
  end
  object grp1: TGroupBox
    Left = 592
    Top = 472
    Width = 337
    Height = 113
    Caption = 'Range'
    TabOrder = 5
    object btnCheckOneRange: TButton
      Left = 16
      Top = 24
      Width = 145
      Height = 25
      Caption = 'Check 1 GB18030 Range'
      TabOrder = 0
      OnClick = btnCheckOneRangeClick
    end
    object btnCheckAllRange: TButton
      Left = 176
      Top = 24
      Width = 145
      Height = 25
      Caption = 'Check All GB18030 Range'
      TabOrder = 1
      OnClick = btnCheckAllRangeClick
    end
    object btnGenGB18030PagePartly2: TButton
      Left = 16
      Top = 64
      Width = 305
      Height = 25
      Caption = 'Generate GB18030 Utf16 4 Inc'
      TabOrder = 2
      OnClick = btnGenGB18030PagePartly2Click
    end
  end
  object grpPuaNTS: TGroupBox
    Left = 944
    Top = 312
    Width = 201
    Height = 185
    Caption = 'NITS && PUA'
    TabOrder = 6
    object btnGenGB18030PuaUtf16: TButton
      Left = 16
      Top = 24
      Width = 169
      Height = 25
      Caption = 'Generate GB18030 PUA Utf16'
      TabOrder = 0
      OnClick = btnGenGB18030PuaUtf16Click
    end
    object btnGenGB18030Utf16Pua: TButton
      Left = 16
      Top = 56
      Width = 169
      Height = 25
      Caption = 'Generate GB18030 Utf16 PUA'
      TabOrder = 1
      OnClick = btnGenGB18030Utf16PuaClick
    end
    object btnGenGB18030UnicodeMapBMP: TButton
      Left = 16
      Top = 112
      Width = 169
      Height = 25
      Caption = 'Generate GB18030 Unicode BMP '
      TabOrder = 2
      OnClick = btnGenGB18030UnicodeMapBMPClick
    end
    object btnGenGB18030UnicodeMapSMP: TButton
      Left = 16
      Top = 144
      Width = 169
      Height = 25
      Caption = 'Generate GB18030 Unicode SMP '
      TabOrder = 3
      OnClick = btnGenGB18030UnicodeMapSMPClick
    end
  end
  object dlgSave1: TSaveDialog
    FileName = 'CHARS.txt'
    Left = 928
    Top = 40
  end
end
