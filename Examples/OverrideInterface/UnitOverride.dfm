object FormTest: TFormTest
  Left = 291
  Top = 241
  BorderStyle = bsDialog
  Caption = 'Test for Interfaces'
  ClientHeight = 345
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 24
    Top = 24
    Width = 417
    Height = 297
    Lines.Strings = (
      '测试对一未知接口的动态判断与覆盖。'
      ''
      
        '场景：Delphi 10.2.3 中的 ToolsAPI 较 10.2.2 添加了主题相关支持的' +
        '接口，专家'
      '包如果要支持主题，则必须使用 10.2.3 编译，并使用这批接口。 '
      ' '
      
        '带来的问题是，如果专家包在 10.2.2 下运行，则会因为无法找到这些接' +
        '口而出'
      '错。'
      ''
      
        '现在改用另外一种方案，不直接引用 10.2.3 的那批新接口，只用其 GUI' +
        'D 进行 '
      'Supports 调用查询，如 10.2.2，会查询不到接口的实现，从而可禁用。'
      ''
      
        '如果查到了实现，也不能直接使用 10.2.3 的那批新接口，只能仿照那批' +
        '接口写'
      '一套对应的一模一样的接口，然后调用。'
      ''
      
        '本例子演示并验证了这种调用，原始接口提供者是 IOriginalInterface ' +
        '以及其实现'
      '，并将实现通过 IUnknown 接口返回。'
      ''
      
        '调用者声明了一个一模一样的接口 IOverrideProvider，通过Supports ' +
        '查询 '
      
        'IOriginalInterface 的 GUID，获得其实现并强制转换成 IOverrideProv' +
        'ider 以成功调'
      '用。完全不用到 IOriginalInterface 的声明。')
    TabOrder = 0
  end
end
