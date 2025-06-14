program TestJsonRpc;
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Contnrs, CnRPC, CnJSON;

const
  CN_INVALID_ID             = -1;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

procedure AssertTrue(Condition: Boolean; const Msg: string);
begin
  Inc(TotalTests);
  if Condition then
    Inc(PassedTests)
  else
  begin
    Inc(FailedTests);
    Writeln('FAIL: ', Msg);
  end;
end;

procedure AssertFalse(Condition: Boolean; const Msg: string);
begin
  AssertTrue(not Condition, Msg);
end;

procedure AssertEquals(Expected, Actual: Integer; const Msg: string);
begin
  AssertTrue(Expected = Actual, Format('%s (Expected: %d, Actual: %d)', 
    [Msg, Expected, Actual]));
end;

procedure AssertEqualsStr(const Expected, Actual: AnsiString; const Msg: string);
begin
  AssertTrue(Expected = Actual, Format('%s (Expected: "%s", Actual: "%s")', 
    [Msg, Expected, Actual]));
end;

procedure AssertNotNull(Obj: TObject; const Msg: string);
begin
  AssertTrue(Obj <> nil, Msg);
end;

procedure AssertNull(Obj: TObject; const Msg: string);
begin
  AssertTrue(Obj = nil, Msg);
end;

procedure AssertIs(Obj: TObject; Cls: TClass; const Msg: string);
begin
  AssertTrue((Obj <> nil) and (Obj.ClassType = Cls), 
    Format('%s (Expected class: %s)', [Msg, Cls.ClassName]));
end;

{ 测试用例实现 }
procedure TestRequestToJSON;
var
  Request: TCnJSONRPCRequest;
  Json: AnsiString;
begin
  Request := TCnJSONRPCRequest.Create;
  try
    Request.Method := 'add';
    Request.Params := TCnJSONArray.Create;
    TCnJSONArray(Request.Params).AddValue(5);
    TCnJSONArray(Request.Params).AddValue(3);
    Request.ID := 1;

    Json := Request.ToJSON(False);
    AssertEqualsStr(
      '{"jsonrpc":"2.0","method":"add","params":[5,3],"id":1}',
      Json,
      'TestRequestToJSON: 生成的JSON不符合预期'
    );
  finally
    Request.Free;
  end;
end;

procedure TestResponseToJSON;
var
  Response: TCnJSONRPCResponse;
  Json: AnsiString;
begin
  Response := TCnJSONRPCResponse.Create;
  try
    Response.RPCResult := TCnJSONNumber.FromInt(8);
    Response.ID := 1;

    Json := Response.ToJSON(False);
    AssertEqualsStr(
      '{"jsonrpc":"2.0","result":8,"id":1}',
      Json,
      'TestResponseToJSON: 生成的JSON不符合预期'
    );
  finally
    Response.Free;
  end;
end;

procedure TestErrorToJSON;
var
  Error: TCnJSONRPCError;
  Json: AnsiString;
begin
  Error := TCnJSONRPCError.Create;
  try
    Error.ErrorCode := -32601;
    Error.ErrorMessage := 'Method not found';
    Error.ID := 1;

    Json := Error.ToJSON(False);
    AssertEqualsStr(
      '{"jsonrpc":"2.0","error":{"code":-32601,"message":"Method not found","data":{}},"id":1}',
      Json,
      'TestErrorToJSON: 生成的JSON不符合预期'
    );
  finally
    Error.Free;
  end;
end;

procedure TestNotificationToJSON;
var
  Notification: TCnJSONRPCNoficiation;
  Json: AnsiString;
begin
  Notification := TCnJSONRPCNoficiation.Create;
  try
    Notification.Method := 'update';
    Notification.Params := TCnJSONObject.Create;
    TCnJSONObject(Notification.Params).AddPair('status', 'ready');

    Json := Notification.ToJSON(False);
    AssertEqualsStr(
      '{"jsonrpc":"2.0","method":"update","params":{"status":"ready"}}',
      Json,
      'TestNotificationToJSON: 生成的JSON不符合预期'
    );
  finally
    Notification.Free;
  end;
end;

procedure TestParseRequest;
var
  Json: AnsiString;
  RPC: TCnJSONRPCBase;
begin
  Json := '{"jsonrpc":"2.0","method":"subtract","params":[10,3],"id":2}';
  RPC := CnParseJSONRPC(Json);
  try
    AssertNotNull(RPC, 'TestParseRequest: 解析返回nil');
    AssertIs(RPC, TCnJSONRPCRequest, 'TestParseRequest: 不是请求类型');
    AssertEqualsStr('subtract', TCnJSONRPCRequest(RPC).Method, 'TestParseRequest: 方法名不匹配');
    AssertEquals(2, RPC.ID, 'TestParseRequest: ID不匹配');
  finally
    if RPC <> nil then RPC.Free;
  end;
end;

procedure TestParseResponse;
var
  Json: AnsiString;
  RPC: TCnJSONRPCBase;
begin
  Json := '{"jsonrpc":"2.0","result":7,"id":2}';
  RPC := CnParseJSONRPC(Json);
  try
    AssertNotNull(RPC, 'TestParseResponse: 解析返回nil');
    AssertIs(RPC, TCnJSONRPCResponse, 'TestParseResponse: 不是响应类型');
    AssertEquals(7, TCnJSONRPCResponse(RPC).RPCResult.AsInteger, 
      'TestParseResponse: 结果值不匹配');
  finally
    if RPC <> nil then RPC.Free;
  end;
end;

procedure TestParseError;
var
  Json: AnsiString;
  RPC: TCnJSONRPCBase;
begin
  Json := '{"jsonrpc":"2.0","error":{"code":-32601,"message":"Method not found"},"id":1}';
  RPC := CnParseJSONRPC(Json);
  try
    AssertNotNull(RPC, 'TestParseError: 解析返回nil');
    AssertIs(RPC, TCnJSONRPCError, 'TestParseError: 不是错误类型');
    AssertEquals(-32601, TCnJSONRPCError(RPC).ErrorCode, 'TestParseError: 错误码不匹配');
    AssertEqualsStr('Method not found', TCnJSONRPCError(RPC).ErrorMessage, 
      'TestParseError: 错误信息不匹配');
  finally
    if RPC <> nil then RPC.Free;
  end;
end;

procedure TestParseNotification;
var
  Json: AnsiString;
  RPC: TCnJSONRPCBase;
begin
  Json := '{"jsonrpc":"2.0","method":"heartbeat","params":{}}';
  RPC := CnParseJSONRPC(Json);
  try
    AssertNotNull(RPC, 'TestParseNotification: 解析返回nil');
    AssertIs(RPC, TCnJSONRPCNoficiation, 'TestParseNotification: 不是通知类型');
    AssertEqualsStr('heartbeat', TCnJSONRPCNoficiation(RPC).Method, 
      'TestParseNotification: 方法名不匹配');
    AssertEquals(CN_INVALID_ID, RPC.ID, 'TestParseNotification: ID应为无效值');
  finally
    if RPC <> nil then RPC.Free;
  end;
end;

procedure TestParseBatch;
var
  Json: AnsiString;
  RPCs: TObjectList;
begin
  Json := '[{"jsonrpc":"2.0","method":"ping","id":1},' +
           '{"jsonrpc":"2.0","result":"pong","id":1}]';
  RPCs := TObjectList.Create(True);
  try
    AssertTrue(CnParseJSONRPCs(Json, RPCs), 'TestParseBatch: 批量解析失败');
    AssertEquals(2, RPCs.Count, 'TestParseBatch: 解析对象数量不符');
    AssertIs(RPCs[0], TCnJSONRPCRequest, 'TestParseBatch: 第一个对象不是请求');
    AssertIs(RPCs[1], TCnJSONRPCResponse, 'TestParseBatch: 第二个对象不是响应');
  finally
    RPCs.Free;
  end;
end;

procedure TestParseEmptyString;
var
  RPC: TCnJSONRPCBase;
begin
  RPC := CnParseJSONRPC('');
  AssertNull(RPC, 'TestParseEmptyString: 空字符串应返回nil');
end;

procedure TestParseInvalidJSON;
var
  RPC: TCnJSONRPCBase;
begin
  try
    RPC := CnParseJSONRPC('{invalid json}');
  except
    RPC := nil;
  end;
  AssertNull(RPC, 'TestParseInvalidJSON: 无效JSON应返回nil');
end;

procedure TestParseMissingRequiredFields;
var
  RPC: TCnJSONRPCBase;
begin
  // 缺少method的请求
  RPC := CnParseJSONRPC('{"jsonrpc":"2.0","id":1}');
  AssertNull(RPC, 'TestParseMissingRequiredFields: 缺少method应返回nil');
end;

procedure TestMaxIntegerID;
var
  Request: TCnJSONRPCRequest;
  Json: AnsiString;
  RPC: TCnJSONRPCBase;
const
  MAX_ID = 2147483647;
begin
  Request := TCnJSONRPCRequest.Create;
  try
    Request.Method := 'test';
    Request.ID := MAX_ID;
    Json := Request.ToJSON(False);

    RPC := CnParseJSONRPC(Json);
    try
      AssertEquals(MAX_ID, RPC.ID, 'TestMaxIntegerID: 最大ID值解析失败');
    finally
      if RPC <> nil then RPC.Free;
    end;
  finally
    Request.Free;
  end;
end;

procedure TestEmptyParams;
var
  Json: AnsiString;
  RPC: TCnJSONRPCBase;
begin
  Json := '{"jsonrpc":"2.0","method":"noParams","id":1}';
  RPC := CnParseJSONRPC(Json);
  try
    AssertNotNull(RPC, 'TestEmptyParams: 解析返回nil');
    AssertIs(RPC, TCnJSONRPCRequest, 'TestEmptyParams: 不是请求类型');
    AssertTrue(TCnJSONRPCRequest(RPC).Params = nil, 'TestEmptyParams: 参数应为nil');
  finally
    if RPC <> nil then RPC.Free;
  end;
end;

{ 主测试程序 }
begin
  try
    Writeln('Starting CnRPC unit tests...');
    Writeln('---------------------------');
    
    // 执行所有测试用例
    TestRequestToJSON;
    TestResponseToJSON;
    TestErrorToJSON;
    TestNotificationToJSON;
    TestParseRequest;
    TestParseResponse;
    TestParseError;
    TestParseNotification;
    TestParseBatch;
    TestParseEmptyString;
    TestParseInvalidJSON;
    TestParseMissingRequiredFields;
    TestMaxIntegerID;
    TestEmptyParams;
    
    // 输出测试结果
    Writeln;
    Writeln('Test Results:');
    Writeln('-------------');
    Writeln('Total Tests: ', TotalTests);
    Writeln('Passed:      ', PassedTests);
    Writeln('Failed:      ', FailedTests);
    
    if FailedTests > 0 then
    begin
      Writeln;
      Writeln('!!! SOME TESTS FAILED !!!');
    end
    else
    begin
      Writeln;
      Writeln('ALL TESTS PASSED!');
    end;
    
    Writeln;
    Writeln('Press Enter to exit...');
    Readln;
  except
    on E: Exception do
    begin
      Writeln('Unhandled exception: ', E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;
end.
