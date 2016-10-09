{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnRedisClient;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯组件包 Redis 客户端实现单元
* 单元作者：CnPack开发组 CodeGame/ReverseKing
* 备    注：
* 开发平台：PWinXP + Delphi XE
* 兼容测试：PWinXP/7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2016.09.10 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF COMPILER7_UP}
  {$MESSAGE ERROR 'CnRedisClient only Supports Delphi 7 or above.'}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, TypInfo, Graphics, ScktComp, Contnrs;

const
  SCN_REDIS_CRLF = Char($0D) + Char($0A);
  SCN_REDISD_EFAULT_BUFFER_SIZE = 32 * 1024; //32K
  SCN_REDIS_INFO_SECTION_NAME: array[0..11] of string = ('default', 'all',
    'server', 'clients', 'memory', 'persistence', 'stats', 'replication', 'cpu',
    'commandstats', 'cluster', 'keyspace');
  SCN_REDIS_DATA_TYPE_NAME: array[0..7] of string = ('none', 'string', 'list',
    'set', 'zset', 'hash', 'integer', 'msg');
  SCN_REDIS_OPERATION: array[0..3] of string = ('AND', 'OR', 'XOR', 'NOT');
  SCN_REDIS_EMPTY_VALUE = '(empty)';

type
  ICnRedisSocket = interface(IInterface)
    ['{C5C77E7F-9483-4C68-AED5-45D9398EB425}']
    procedure SetHost(Value: string);
    function GetHost: string;
    procedure SetPort(Value: Word);
    function GetPort: Word;
    procedure SetPassword(Value: string);
    function GetPassword: string;
    procedure SetConnecting(Value: Boolean);
    function GetConnecting: Boolean;
    function Connect: Boolean;
    procedure Disconnect;
    function SendBuffer(Buffer: Pointer; Length: Cardinal): Integer;
    function RecvBuffer(Buffer: Pointer; Length: Cardinal): Integer;
    property RedisHost: string read GetHost write SetHost;
    property RedisPort: Word read GetPort write SetPort;
    property Password: string read GetPassword write SetPassword;
    property Connecting: Boolean read GetConnecting write SetConnecting;
  end;

  TCnRedisAbstractSocket = class(TInterfacedPersistent, ICnRedisSocket)
  private
  protected
    FPassword: string;
    FConnecting: Boolean;
    procedure SetHost(Value: string); virtual; abstract;
    function GetHost: string; virtual; abstract;
    procedure SetPort(Value: Word); virtual; abstract;
    function GetPort: Word; virtual; abstract;
    procedure SetPassword(Value: string); virtual; abstract;
    function GetPassword: string; virtual; abstract;
    procedure SetConnecting(Value: Boolean); virtual; abstract;
    function GetConnecting: Boolean; virtual; abstract;
    function SendBuffer(Buffer: Pointer; Length: Cardinal): Integer; virtual; abstract;
    function RecvBuffer(Buffer: Pointer; Length: Cardinal): Integer; virtual; abstract;
  public
    constructor Create; virtual; abstract;
    function Connect: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    property RedisHost: string read GetHost write SetHost;
    property RedisPort: Word read GetPort write SetPort;
    property Password: string read GetPassword write SetPassword;
    property Connecting: Boolean read GetConnecting write SetConnecting;
  end;

  TCnRedisAbstractSocketType = class of TCnRedisAbstractSocket;

  TCnRedisBuffer = array of Byte;

  TCnRedisDataType = (rdt_none, rdt_string, rdt_list, rdt_set, rdt_zset,
    rdt_hash, rdt_integer, rdt_msg);

  TCnRedisInfoSection = (ris_default, ris_all, ris_server, ris_clients,
    ris_memory, ris_persistence, ris_stats, ris_replication, ris_cpu,
    ris_commandstats, ris_cluster, ris_keyspace);

  TCnRedisOperation = (ro_AND, ro_OR, ro_XOR, or_NOT);

  TCnRedisKeyValue = packed record
    Key: string;
    Value: string;
  end;

  TCnRedisKeyValueArray = array of TCnRedisKeyValue;

  PCnRedisMultiBulkNode = ^TCnRedisMultiBulkNode;

  TCnRedisMultiBulkNode = class
  private
    FCurrIndex: Integer;
    FParent: TCnRedisMultiBulkNode;
    FMultiBulkRefs: TObjectList;
    FValue: string;
  public
    constructor Create;
    destructor Destroy; override;
    // 类似于 SetLength，保证每个元素都是有效的 Node，多时增加，少时释放
    procedure ChangeMultiBulksSize(NewCount: Integer);

    property CurrIndex: Integer read FCurrIndex write FCurrIndex;
    property Parent: TCnRedisMultiBulkNode read FParent write FParent;
    property Value: string read FValue write FValue;
    property MultiBulkRefs: TObjectList read FMultiBulkRefs write FMultiBulkRefs;
  end;

  TCnRedisMultiBulk = TCnRedisMultiBulkNode;

  TCnRedisClientSocket = class(TCnRedisAbstractSocket)
  protected
    FSocket: TClientSocket;
    procedure SetHost(Value: string); override;
    function GetHost: string; override;
    procedure SetPort(Value: Word); override;
    function GetPort: Word; override;
    procedure SetPassword(Value: string); override;
    function GetPassword: string; override;
    procedure SetConnecting(Value: Boolean); override;
    function GetConnecting: Boolean; override;
    procedure OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    function SendBuffer(Buffer: Pointer; Length: cardinal): Integer; override;
    function RecvBuffer(Buffer: Pointer; Length: cardinal): Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Connect: Boolean; override;
    procedure Disconnect; override;
  end;

  ICnRedisCommand280 = interface(IInterface)
    ['{3F9E4F71-0F9D-43F9-B4D2-6A6284E5EFFE}']
{$IFDEF IDE_EDITOR_SUPPORTS_FOLDING}
    {$REGION '==========Redis Command v2.8=========='}
{$ENDIF}
    //--------------------------KEY----------------------------//
    function DEL(const Keys: string): Int64; //删除给定的一个或多个key,返回值：被删除key的数量,不存在返回0。
    function DUMP(const Key: string): string; //列化给定 key ，并返回被序列化的值，使用 RESTORE 命令可以将这个值反序列化为 Redis 键。
    function EXISTS(const Key: string): Boolean;  //检查给定 key 是否存在。
    function EXPIRE(const Key: string; Seconds: Int64): Boolean; //为给定 key 设置生存时间，当 key 过期时(生存时间为 0 )，它会被自动删除,单位秒。
    function EXPIREAT(const Key: string; UnixTime: Int64): Boolean; //功能和EXPIRE一样，命令接受的时间参数是 UNIX 时间戳(unix timestamp)。
    function KEYS(const pattern: string; Value: TStrings): Integer; //查找所有符合给定模式 pattern 的 key
    function MIGRATE(const Host: string; Port: Word; const Key: string; DestDB,
      Timeout: Cardinal; IsCopy, IsReplace: Boolean): Boolean; //将 key 原子性地从当前实例传送到目标实例的指定数据库上
    function MOVE(const Key: string; DB: Integer): Boolean; //将当前数据库的 key 移动到给定的数据库 db 当中。
    function OBJECTREFCOUNT(const key: string): Integer; // 返回给定 key 引用所储存的值的次数。此命令主要用于除错。
    function OBJECTENCODING(const key: string): string; // 返回给定 key 锁储存的值所使用的内部表示(representation)。
    function OBJECTIDLETIME(const key: string): Integer; // 返回给定 key 自储存以来的空转时间(idle， 没有被读取也没有被写入)，以秒为单位。
    function PERSIST(const Key: string): Boolean;  //移除给定 key 的生存时间，将这个 key 从『易失的』(带生存时间 key )转换成『持久的』(一个不带生存时间、永不过期的 key )。
    function PEXPIRE(const Key: string; MilliSeconds: Int64): Boolean;  //为给定 key 设置生存时间，当 key 过期时(生存时间为 0 )，它会被自动删除,单位毫秒。
    function PEXPIREAT(const Key: string; UnixTime: Int64): Boolean; //功能和EXPIREAT一样，但它以毫秒为单位设置 key 的过期 unix 时间戳。
    function TTL(const Key: string): Int64; //以秒为单位，返回给定 key 的剩余生存时间
    function PTTL(const Key: string): Int64; //这个命令类似于 TTL 命令，但它以毫秒为单位返回 key 的剩余生存时间，而不是像 TTL 命令那样，以秒为单位。
    function RANDOMKEY: string; //从当前数据库中随机返回(不删除)一个 key 。
    function RENAME(const Key, NewKey: string): Boolean; //将key改名为newkey。当key和newkey相同或者key不存在时，返回一个错误。当newkey已经存在时，RENAME命令将覆盖旧值。
    function RENAMENX(const Key, NewKey: string): Boolean; //当且仅当 newkey 不存在时，将 key 改名为 newkey 。
    function RESTORE(const Key: string; ttl: Cardinal; const SValue: string):
      Boolean; //反序列化给定的序列化值，并将它和给定的key关联。参数ttl以毫秒为单位为key设置生存时间；如果ttl为0 ，那么不设置生存时间。
    function SORT(const Key, Param: string; Value: TStrings): Integer; //返回或保存给定列表、集合、有序集合 key 中经过排序的元素。
    function _TYPE(const Key: string): TCnRedisDataType;
    function SCAN(cursor: Integer; MATCH: string; Count: Integer):
      TCnRedisMultiBulk; //2.8+

    //-------------------------String---------------------------//
    function APPEND(const Key, Value: string): Integer; //如果 key 已经存在并且是一个字符串， APPEND 命令将 value 追加到 key 原来的值的末尾。如果 key 不存在， APPEND 就简单地将给定 key 设为 value ，就像执行 SET key value 一样。
    function BITCOUNT(const Key: string; Start: Integer = 0; Stop: Integer = 0):
      Integer; //计算给定字符串中，被设置为 1 的比特位的数量,返回被设置为 1 的位的数量。
    function BITOP(operation: TCnRedisOperation; const DestKey, Keys: string):
      Integer; //对一个或多个保存二进制位的字符串 key 进行位元操作，并将结果保存到 destkey 上。
    function DECR(const Key: string): Int64; //将 key 中储存的数字值减一。
    function DECRBY(const Key: string; Decrement: Int64): Int64; //将 key 所储存的值减去减量 decrement 。
    function GETRANGE(const Key: string; Start, Stop: Integer): string; //返回 key 中字符串值的子字符串，字符串的截取范围由 start 和 end 两个偏移量决定(包括 start 和 end 在内)。
    function GET(const Key: string): string;  //返回 key 所关联的字符串值,如果key不存在那么返回特殊值nil 。
    function GETBIT(const Key: string; offset: Integer): Integer;
    function _SET(const Key, Value: string; EXSecond: Cardinal; Exist: Byte):
      Boolean; //将字符串值value关联到key,EXSecond键值存在时长单位秒,Exist，0：无论是否存在都设置，1：存在时候设置，2：不存在时候设置,成功返回true
    function GETSET(const Key, Value: string): string; //将给定 key 的值设为 value ，并返回 key 的旧值(old value)。当 key 存在但不是字符串类型时，返回一个错误。
    function INCR(const Key: string): Int64;  //将 key 中储存的数字值增一。如果 key 不存在，那么 key 的值会先被初始化为 0 ，然后再执行 INCR 操作。返回执行 INCR 命令之后 key 的值。
    function INCRBY(const Key: string; Increment: Int64): Int64;  //将 key 所储存的值加上增量 Increment 。返回加上 Increment 之后， key 的值。
    function INCRBYFLOAT(const Key: string; Increment: Single): Single;  //将 key 所储存的值加上浮点增量 Increment 。返回加上 Increment 之后， key 的浮点值。
    function MGET(const Keys: string; Value: TStrings): Integer; //返回所有(一个或多个)给定 key 的值。
    function MSET(const KVs: string): Boolean; //同时设置一个或多个 key-value 对，总是返回true
    function MSETNX(const KVs: string): Boolean; //同时设置一个或多个 key-value 对，当且仅当所有给定 key 都不存在。即使只有一个给定 key 已存在， MSETNX 也会拒绝执行所有给定 key 的设置操作。
    function PSETEX(const Key, Value: string; MilliSeconds: Int64): Boolean;  //这个命令和 SETEX 命令相似，但它以毫秒为单位设置 key 的生存时间，而不是像 SETEX 命令那样，以秒为单位。
    function SETBIT(const Key: string; Offset, Value: Integer): Integer; //对 key 所储存的字符串值，设置或清除指定偏移量上的位(bit)。
    function SETEX(const Key, Value: string; Seconds: Int64): Boolean; //将值 value 关联到 key ，并将 key 的生存时间设为 seconds (以秒为单位)。如果 key 已经存在， SETEX 命令将覆写旧值。
    function SETNX(const Key, Value: string): Boolean; //将 key 的值设为 value ，当且仅当 key 不存在。若给定的 key 已经存在，则 SETNX 不做任何动作。
    function SETRANGE(const Key, value: string; Offset: Integer): Integer; //用 value 参数覆写(overwrite)给定 key 所储存的字符串值，从偏移量 Offset 开始。不存在的 key 当作空白字符串处理,返回被 SETRANGE 修改之后，字符串的长度。
    function STRLEN(const Key: string): Integer; //返回 key 所储存的字符串值的长度。当 key 储存的不是字符串值时，返回一个错误。

    //-------------------------HASH---------------------------//
    function HDEL(const Key, Fields: string): Integer; //删除哈希表 key 中的一个或多个指定域，不存在的域将被忽略。返回被删除的数量。
    function HEXISTS(const Key, Field: string): Boolean; //查看哈希表 key 中，给定域 Field 是否存在。
    function HGET(const Key, Field: string): string; //返回哈希表 key 中给定域 Field 的值。
    function HGETALL(const Key: string; var Value: TCnRedisKeyValueArray): Integer;
    function HINCRBY(const Key, Field: string; Increment: Int64): Int64; //为哈希表 key 中的域 Field 的值加上增量 Increment 。增量也可以为负数，相当于对给定域进行减法操作。返回执行 HINCRBY 命令之后，哈希表 key 中域 Field 的值。
    function HINCRBYFLOAT(const Key, Field: string; Increment: Single): Single; //为哈希表 key 中的域 Field 的值加上增量 Increment 。增量也可以为负数，相当于对给定域进行减法操作。返回执行 HINCRBY 命令之后，哈希表 key 中域 Field 的值。
    function HKEYS(const Key: string; Value: TStrings): Integer; //返回哈希表 key 中的所有域。
    function HLEN(const Key: string): Integer; //返回哈希表 key 中域的数量。
    function HMGET(const Key, Fields: string; Value: TStrings): Integer; //返回哈希表 key 中，一个或多个给定域的值。
    function HMSET(const Key, fvs: string): Boolean; //同时将多个 Field-Value (域-值)对设置到哈希表 key 中。此命令会覆盖哈希表中已存在的域。
    function HSET(const Key, Field, Value: string): Boolean; //将哈希表 key 中的域 Field 的值设为value。如果field是哈希表中的一个新建域，并且值设置成功，返回1。如果哈希表中域field已经存在且旧值已被新值覆盖，返回0。
    function HSETNX(const Key, Field, Value: string): Boolean; //将哈希表 key 中的域 Field 的值设置为 Value ，当且仅当域 Field 不存在。若域 Field 已经存在，该操作无效。
    function HVALS(const Key: string; Value: TStrings): Integer; //返回哈希表 key 中所有域的值。
    function HSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk;

    //-------------------------LIST---------------------------//
    function BLPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue; //它是 LPOP 命令的阻塞版本，当给定列表内没有任何元素可供弹出的时候，连接将被 BLPOP 命令阻塞，直到等待超时或发现可弹出元素为止。
    function BRPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue; //它是 RPOP 命令的阻塞版本，当给定列表内没有任何元素可供弹出的时候，连接将被 BRPOP 命令阻塞，直到等待超时或发现可弹出元素为止。
    function BRPOPLPUSH(const source, Destination: string; Timeout: Cardinal):
      TCnRedisKeyValue; //阻塞版，将列表 source 中的最后一个元素(尾元素)弹出，并返回给客户端。将 source 弹出的元素插入到列表 Destination ，作为 Destination 列表的的头元素。
    function LINDEX(const Key: string; index: Integer): string; //返回列表 key 中，下标为 index 的元素。
    function LINSERT(const Key, Pivot, Value: string; IsBEFORE: Boolean):
      Integer; //将值 Value 插入到列表 key 当中，位于值 Pivot 之前或之后。当 Pivot 不存在于列表 key 时，不执行任何操作。
    function LLEN(const Key: string): Integer; //返回列表 key 的长度。
    function LPOP(const Key: string): string; //移除并返回列表 key 的头元素。
    function LPUSH(const Key, values: string): Integer; //将一个或多个值 Value 插入到列表 key 的表头
    function LPUSHX(const Key, Value: string): Integer; //将值 Value 插入到列表 key 的表头，当且仅当 key 存在并且是一个列表。和 LPUSH 命令相反，当 key 不存在时， LPUSHX 命令什么也不做。
    function LRANGE(const Key: string; Start, Stop: Integer; Value: TStrings):
      Integer; //返回列表 key 中指定区间内的元素，区间以偏移量 Start 和 Stop 指定。
    function LREM(const Key, Value: string; Count: Integer): Integer;  //根据参数 Count 的值，移除列表中与参数 Value 相等的元素。
    function LSET(const Key, Value: string; Index: Integer): Boolean;   //将列表 key 下标为 index 的元素的值设置为 Value 。
    function LTRIM(const Key: string; Start, Stop: Integer): Boolean;
    function RPOP(const Key: string): string; //移除并返回列表 key 的尾元素。
    function RPOPLPUSH(const source, Destination: string): string;
    function RPUSH(const Key, values: string): Integer; //将一个或多个值 Value 插入到列表 key 的表尾(最右边)。
    function RPUSHX(const Key, Value: string): Integer; //将值 Value 插入到列表 key 的表尾，当且仅当 key 存在并且是一个列表。和 RPUSH 命令相反，当 key 不存在时，RPUSHX命令什么也不做。

    //-------------------------Set---------------------------//
    function SADD(const Key, Members: string): Integer; //将一个或多个 Member 元素加入到集合 key 当中，已经存在于集合的 Member 元素将被忽略。
    function SCARD(const Key: string): Integer; //返回集合 key 的基数(集合中元素的数量)。
    function SDIFF(const Keys: string; Value: TStrings): Integer; //返回一个集合的全部成员，该集合是所有给定集合之间的差集。
    function SDIFFSTORE(const Destination, Keys: string): Integer; //这个命令的作用和 SDIFF 类似，但它将结果保存到 Destination 集合，而不是简单地返回结果集。
    function SINTER(const Keys: string; Value: TStrings): Integer;  //返回一个集合的全部成员，该集合是所有给定集合的交集。
    function SINTERSTORE(const Keys, Destination: string): Integer; //这个命令类似于 SINTER 命令，但它将结果保存到 Destination 集合，而不是简单地返回结果集。
    function SISMEMBER(const Key, Member: string): Boolean; // 判断 Member 元素是否集合 key 的成员。
    function SMEMBERS(const Key: string; Value: TStrings): Integer;   //返回集合 key 中的所有成员
    function SMOVE(const source, Destination, Member: string): Boolean;  //将 Member 元素从 source 集合移动到 Destination 集合。
    function SPOP(const Key: string): string; //移除并返回集合中的一个随机元素。
    function SRANDMEMBER(const Key: string; Count: Integer; Value: TStrings):
      Integer; //如果命令执行时，只提供了 key 参数，那么返回集合中的一个随机元素。
    function SREM(const Key, Members: string): Integer; //移除集合 key 中的一个或多个 Member 元素，不存在的 Member 元素会被忽略。
    function SUNION(const Keys: string; Value: TStrings): Integer;  //返回一个集合的全部成员，该集合是所有给定集合的并集。
    function SUNIONSTORE(const Keys, Destination: string): Integer; // 这个命令类似于 SUNION 命令，但它将结果保存到 Destination 集合，而不是简单地返回结果集。如果 Destination 已经存在，则将其覆盖。
    function SSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; //

    //----------------------SortedSet------------------------//
    function ZADD(const Key, ScoreMembers: string): Integer;  //将一个或多个 Member 元素及其 score 值加入到有序集 key 当中。
    function ZCARD(const Key: string): Integer;   //返回有序集 key 的基数。
    function ZCOUNT(const Key: string; Min, Max: Integer): Integer;  //返回有序集 key 中， score 值在 min 和 max 之间(默认包括 score 值等于 min 或 max )的成员的数量。
    function ZINCRBY(const Key, Member: string; Increment: Single): Single; //为有序集 key 的成员 Member 的 score 值加上增量 increment 。
    function ZRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray;  //返回有序集 key 中，指定区间内的成员。其中成员的位置按 score 值递增(从小到大)来排序。
    function ZRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray;  //返回有序集 key 中，所有 score 值介于 min 和 max 之间(包括等于 min 或 max )的成员。有序集成员按 score 值递增(从小到大)次序排列。
    function ZRANK(const Key, Member: string): Integer; //返回有序集 key 中成员 Member 的排名。其中有序集成员按 score 值递增(从小到大)顺序排列。
    function ZREM(const Key, Members: string): Integer; //移除有序集 key 中的一个或多个成员，不存在的成员将被忽略。
    function ZREMRANGEBYRANK(const Key: string; Start, Stop: Integer): Integer; //移除有序集 key 中，指定排名(rank)区间内的所有成员。
    function ZREMRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; //移除有序集 key 中，所有 score 值介于 min 和 max 之间(包括等于 min 或 max )的成员。
    function ZREVRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; //返回有序集 key 中，指定区间内的成员。
    function ZREVRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; //返回有序集 key 中， score 值介于 max 和 min 之间(默认包括等于 max 或 min )的所有的成员。有序集成员按 score 值递减(从大到小)的次序排列。
    function ZREVRANK(const Key, Member: string): Integer;  //返回有序集 key 中，指定区间内的成员。
    function ZSCORE(const Key, Member: string): string; //返回有序集 key 中，成员 Member 的 score 值。
    function ZUNIONSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; //计算给定的一个或多个有序集的并集，其中给定 key 的数量必须以 numkeys 参数指定，并将该并集(结果集)储存到 Destination 。默认情况下，结果集中某个成员的 score 值是所有给定集下该成员 score 值之 和 。
    function ZINTERSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; //计算给定的一个或多个有序集的交集，其中给定 key 的数量必须以 numkeys 参数指定，并将该交集(结果集)储存到 Destination 。默认情况下，结果集中某个成员的 score 值是所有给定集下该成员 score 值之和.
    function ZSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk;

   //-----------------------Pub/Sub-------------------------//
    function PSUBSCRIBE(const Patterns: string; Value: TStrings): Integer;  //订阅一个或多个符合给定模式的频道。
    function PUBLISH(const Channel, message: string): Integer; //将信息 message 发送到指定的频道 channel 。
    function PUBSUB(const Command, Arguments: string; Value: TStrings): Integer;
      // 查看订阅与发布系统状态的内省命令， 它由数个不同格式的子命令组成
    function PUNSUBSCRIBE(const patterns: string): string; //指示客户端退订所有给定模式。
    function SUBSCRIBE(const Channels: string; Value: TStrings): Integer; //订阅给定的一个或多个频道的信息。
    function UNSUBSCRIBE(const Channels: string; Value: TStrings): Integer;  //指示客户端退订给定的频道。


    //---------------------Transaction-----------------------//
    function DISCARD: Boolean; //取消事务，放弃执行事务块内的所有命令。
    function EXEC(Value: TStrings): Integer; //执行所有事务块内的命令。
    function MULTI: Boolean; //标记一个事务块的开始。
    function UNWATCH: Boolean; //取消 WATCH 命令对所有 key 的监视。
    function WATCH(const Keys: string): Boolean; //监视一个(或多个) key ，如果在事务执行之前这个(或这些) key 被其他命令所改动，那么事务将被打断。

    //------------------------Script-------------------------//
    function EVAL(const Script, Keys, Arg: string; Value: TStrings): Integer; //通过内置的 Lua 解释器，可以使用 EVAL 命令对 Lua 脚本进行求值。
    function EVALSHA(const Sha1, Keys, Arg: string): string; //根据给定的 sha1 校验码，对缓存在服务器中的脚本进行求值。
    function SCRIPTEXISTS(const Scripts: string; Value: TStrings): Integer; //给定一个或多个脚本的 SHA1 校验和，返回一个包含 0 和 1 的列表，表示校验和所指定的脚本是否已经被保存在缓存当中。
    function SCRIPTFLUSH: Boolean; //清除所有 Lua 脚本缓存。
    function SCRIPTKILL: Boolean; //杀死当前正在运行的 Lua 脚本，当且仅当这个脚本没有执行过任何写操作时，这个命令才生效。
    function SCRIPTLOAD(const Script: string): string; //将脚本 script 添加到脚本缓存中，但并不立即执行这个脚本。

    //----------------------Connection-----------------------//
    function AUTH(const Password: string): Boolean; //密码匹配时返回 OK ，否则返回一个错误。
    function ECHO(const Msg: string): string;
    function PING: Boolean; //如果连接正常就返回一个 PONG ，否则返回一个连接错误.
    procedure QUIT; //请求服务器关闭与当前客户端的连接。
    function SELECT(DB: Integer): Boolean;  //切换到指定的数据库，数据库索引号 index 用数字值指定，以 0 作为起始索引值。

    //------------------------Server-------------------------//
    function BGREWRITEAOF: string; //执行一个 AOF文件 重写操作。重写会创建一个当前 AOF 文件的体积优化版本。
    function BGSAVE: string; //在后台异步(Asynchronously)保存当前数据库的数据到磁盘。
    function CLIENTGETNAME: string; //返回 CLIENT SETNAME 命令为连接设置的名字。
    function CLIENTKILL(const IP: string; Port: Word): Boolean; //关闭地址为 ip:port 的客户端。
    function CLIENTLIST(Value: TStrings): Integer;  //以人类可读的格式，返回所有连接到服务器的客户端信息和统计数据。
    function CLIENTSETNAME(const Name: string): Boolean;  //为当前连接分配一个名字。
    function CONFIGGET(const Parameters: string): TCnRedisKeyValueArray; //用于取得运行中的 Redis 服务器的配置参数
    function CONFIGRESETSTAT: Boolean;  //重置 INFO 命令中的某些统计数据
    function CONFIGREWRITE: Boolean; //对启动 Redis 服务器时所指定的 redis.conf 文件进行改写
    function CONFIGSET(const Parameter, Value: string): Boolean; //可以动态地调整 Redis 服务器的配置(configuration)而无须重启。
    function DBSIZE: Int64; //返回当前数据库的 key 的数量。
    function DEBUGOBJECT(const Key: string): string; //DEBUG OBJECT 是一个调试命令，它不应被客户端所使用。
    procedure DEBUGSEGFAULT; //执行一个不合法的内存访问从而让 Redis 崩溃，仅在开发时用于 BUG 模拟。
    function FLUSHALL: Boolean; //清空整个 Redis 服务器的数据(删除所有数据库的所有 key)
    function FLUSHDB: Boolean; //清空当前数据库中的所有 key。
    function INFO(Section: TCnRedisInfoSection; Value: TStrings): Integer; //以一种易于解释（parse）且易于阅读的格式，返回关于 Redis 服务器的各种信息和统计数值。
    function LASTSAVE: Int64; //返回最近一次 Redis 成功将数据保存到磁盘上的时间，以 UNIX 时间戳格式表示。
    function MONITOR: Boolean; //实时打印出 Redis 服务器接收到的命令，调试用。
    function PSYNC(const MASTER_RUN_ID: string; Offset: Integer): string; //用于复制功能(replication)的内部命令。
    function SAVE: Boolean; //执行一个同步保存操作，将当前 Redis 实例的所有数据快照(snapshot)以 RDB 文件的形式保存到硬盘。
    function SHUTDOWN: string; //停止所有客户端如果有至少一个保存点在等待，执行 SAVE 命令如果 AOF 选项被打开，更新 AOF 文件关闭 redis 服务器(server)
    function SLAVEOF(const Host: string; Port: Word): Boolean; //SLAVEOF 命令用于在 Redis 运行时动态地修改复制(replication)功能的行为
    function SLOWLOGLEN(const Parameter: string): Integer; //查看当前日志的数量
    function SLOWLOGGET(const Parameter: string): TCnRedisMultiBulk; //获取参数指定的日志
    function SLOWLOGRESET: Boolean; //清空当前日志
    function SYNC: string; //用于复制功能(replication)的内部命令。
    function TIME: TCnRedisKeyValue;  //一个包含两个字符串的列表： 第一个字符串是当前时间(以 UNIX 时间戳格式表示)，而第二个字符串是当前这一秒钟已经逝去的微秒数。
{$IFDEF IDE_EDITOR_SUPPORTS_FOLDING}
   {$ENDREGION}
{$ENDIF}
  end;

  ICnRedisCommand = interface(ICnRedisCommand280)
    ['{A3162571-61A1-4E6A-8D70-2D77BE43CD6A}']
  end;

  TCnRedisProtocol = class(TInterfacedObject, ICnRedisCommand)
  private
    FInterfacedSocket: ICnRedisSocket;
    FRecvBuffer: TCnRedisBuffer;
    FPipelineMode: Boolean;
    FPipelineBuffer: AnsiString;
    function Serialize(const Value: string; var Serial: AnsiString): Integer;
    function Deserialize(Response: Pointer; Length: Integer; RespNode:
      TCnRedisMultiBulkNode): Boolean;
    procedure SetRecvBufferSize(Value: Cardinal);
    function GetConnecting: Boolean;
    function CreateSocketOfClassName(const Name: string): ICnRedisSocket;
  protected
    function Connect: Boolean; virtual;
    function SendAndReceive(const Send: string; Recv: TCnRedisMultiBulkNode;
      Force: Boolean = False): Boolean; virtual;
    function GetRedisDataTypeOfString(const Value: string): TCnRedisDataType;
  public
    // constructor Create(const SocketClassName: string); virtual; overload;
    constructor Create(ASocketIntf: ICnRedisSocket = nil); virtual;
    destructor Destroy; override;
    procedure Disconnect; virtual;
    procedure PipelineBegin;
    function PipelineEnd(Recv: TCnRedisMultiBulk): Boolean;
    procedure SetRedisServer(const Host: string; Port: Word; const Password: string);
    property Connecting: Boolean read GetConnecting;
{$IFDEF IDE_EDITOR_SUPPORTS_FOLDING}
    {$REGION '==========Redis Command v2.8=========='}
{$ENDIF}
    //--------------------------KEY----------------------------//
    function DEL(const Keys: string): Int64; virtual; abstract;
    function DUMP(const Key: string): string; virtual; abstract;
    function EXISTS(const Key: string): Boolean; virtual; abstract;
    function EXPIRE(const Key: string; Seconds: Int64): Boolean; virtual; abstract;
    function EXPIREAT(const Key: string; Timestamp: Int64): Boolean; virtual; abstract;
    function KEYS(const pattern: string; Value: TStrings): Integer; virtual; abstract;
    function MIGRATE(const Host: string; Port: Word; const Key: string; DestDB,
      Timeout: Cardinal; IsCopy, IsReplace: Boolean): Boolean; virtual; abstract;
    function MOVE(const Key: string; DB: Integer): Boolean; virtual; abstract;
    function OBJECTREFCOUNT(const key: string): Integer; virtual; abstract;
    function OBJECTENCODING(const key: string): string; virtual; abstract;
    function OBJECTIDLETIME(const key: string): Integer; virtual; abstract;
    function PERSIST(const Key: string): Boolean; virtual; abstract;
    function PEXPIRE(const Key: string; MilliSeconds: Int64): Boolean; virtual; abstract;
    function PEXPIREAT(const Key: string; UnixTime: Int64): Boolean; virtual; abstract;
    function TTL(const Key: string): Int64; virtual; abstract;
    function PTTL(const Key: string): Int64; virtual; abstract;
    function RANDOMKEY: string; virtual; abstract;
    function RENAME(const Key, NewKey: string): Boolean; virtual; abstract;
    function RENAMENX(const Key, NewKey: string): Boolean; virtual; abstract;
    function RESTORE(const Key: string; ttl: Cardinal; const SValue: string):
      Boolean; virtual; abstract;
    function SORT(const Key, Param: string; Value: TStrings): Integer; virtual; abstract;
    function _TYPE(const Key: string): TCnRedisDataType; virtual; abstract;
    function SCAN(cursor: Integer; MATCH: string; Count: Integer):
      TCnRedisMultiBulk; virtual; abstract;

    //-------------------------String---------------------------//
    function APPEND(const Key, Value: string): Integer; virtual; abstract;
    function BITCOUNT(const Key: string; Start: Integer = 0; Stop: Integer = 0):
      Integer; virtual; abstract;
    function BITOP(operation: TCnRedisOperation; const DestKey, Keys: string):
      Integer; virtual; abstract;
    function DECR(const Key: string): Int64; virtual; abstract;
    function DECRBY(const Key: string; Decrement: Int64): Int64; virtual; abstract;
    function GETRANGE(const Key: string; Start, Stop: Integer): string; virtual; abstract;
    function GET(const Key: string): string; virtual; abstract;
    function GETBIT(const Key: string; offset: Integer): Integer; virtual; abstract;
    function _SET(const Key, Value: string; EXSecond: Cardinal; Exist: Byte):
      Boolean; virtual; abstract;
    function GETSET(const Key, Value: string): string; virtual; abstract;
    function INCR(const Key: string): Int64; virtual; abstract;
    function INCRBY(const Key: string; Increment: Int64): Int64; virtual; abstract;
    function INCRBYFLOAT(const Key: string; Increment: Single): Single; virtual; abstract;
    function MGET(const Keys: string; Value: TStrings): Integer; virtual; abstract;
    function MSET(const KVs: string): Boolean; virtual; abstract;
    function MSETNX(const KVs: string): Boolean; virtual; abstract;
    function PSETEX(const Key, Value: string; MilliSeconds: Int64): Boolean;
      virtual; abstract;
    function SETBIT(const Key: string; Offset, Value: Integer): Integer; virtual;
      abstract;
    function SETEX(const Key, Value: string; Seconds: Int64): Boolean; virtual; abstract;
    function SETNX(const Key, Value: string): Boolean; virtual; abstract;
    function SETRANGE(const Key, value: string; Offset: Integer): Integer;
      virtual; abstract;
    function STRLEN(const Key: string): Integer; virtual; abstract;

    //-------------------------HASH---------------------------//
    function HDEL(const Key, Fields: string): Integer; virtual; abstract;
    function HEXISTS(const Key, Field: string): Boolean; virtual; abstract;
    function HGET(const Key, Field: string): string; virtual; abstract;
    function HGETALL(const Key: string; var Value: TCnRedisKeyValueArray):
      Integer; virtual; abstract;
    function HINCRBY(const Key, Field: string; Increment: Int64): Int64; virtual;
      abstract;
    function HINCRBYFLOAT(const Key, Field: string; Increment: Single): Single;
      virtual; abstract;
    function HKEYS(const Key: string; Value: TStrings): Integer; virtual; abstract;
    function HLEN(const Key: string): Integer; virtual; abstract;
    function HMGET(const Key, Fields: string; Value: TStrings): Integer; virtual;
      abstract;
    function HMSET(const Key, fvs: string): Boolean; virtual; abstract;
    function HSET(const Key, Field, Value: string): Boolean; virtual; abstract;
    function HSETNX(const Key, Field, Value: string): Boolean; virtual; abstract;
    function HVALS(const Key: string; Value: TStrings): Integer; virtual; abstract;
    function HSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; virtual; abstract;

    //-------------------------LIST---------------------------//
    function BLPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue;
      virtual; abstract;
    function BRPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue;
      virtual; abstract;
    function BRPOPLPUSH(const source, Destination: string; Timeout: Cardinal):
      TCnRedisKeyValue; virtual; abstract;
    function LINDEX(const Key: string; index: Integer): string; virtual; abstract;
    function LINSERT(const Key, Pivot, Value: string; IsBEFORE: Boolean):
      Integer; virtual; abstract;
    function LLEN(const Key: string): Integer; virtual; abstract;
    function LPOP(const Key: string): string; virtual; abstract;
    function LPUSH(const Key, values: string): Integer; virtual; abstract;
    function LPUSHX(const Key, Value: string): Integer; virtual; abstract;
    function LRANGE(const Key: string; Start, Stop: Integer; Value: TStrings):
      Integer; virtual; abstract;
    function LREM(const Key, Value: string; Count: Integer): Integer; virtual; abstract;
    function LSET(const Key, Value: string; Index: Integer): Boolean; virtual; abstract;
    function LTRIM(const Key: string; Start, Stop: Integer): Boolean; virtual; abstract;
    function RPOP(const Key: string): string; virtual; abstract;
    function RPOPLPUSH(const source, Destination: string): string; virtual; abstract;
    function RPUSH(const Key, values: string): Integer; virtual; abstract;
    function RPUSHX(const Key, Value: string): Integer; virtual; abstract;

    //-------------------------Set---------------------------//
    function SADD(const Key, Members: string): Integer; virtual; abstract;
    function SCARD(const Key: string): Integer; virtual; abstract;
    function SDIFF(const Keys: string; Value: TStrings): Integer; virtual; abstract;
    function SDIFFSTORE(const Keys, Destination: string): Integer; virtual; abstract;
    function SINTER(const Keys: string; Value: TStrings): Integer; virtual; abstract;
    function SINTERSTORE(const Destination, Keys: string): Integer; virtual; abstract;
    function SISMEMBER(const Key, Member: string): Boolean; virtual; abstract;
    function SMEMBERS(const Key: string; Value: TStrings): Integer; virtual; abstract;
    function SMOVE(const source, Destination, Member: string): Boolean; virtual; abstract;
    function SPOP(const Key: string): string; virtual; abstract;
    function SRANDMEMBER(const Key: string; Count: Integer; Value: TStrings):
      Integer; virtual; abstract;
    function SREM(const Key, Members: string): Integer; virtual; abstract;
    function SUNION(const Keys: string; Value: TStrings): Integer; virtual; abstract;
    function SUNIONSTORE(const Keys, Destination: string): Integer; virtual; abstract;
    function SSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; virtual; abstract;

    //----------------------SortedSet------------------------//
    function ZADD(const Key, ScoreMembers: string): Integer; virtual; abstract;
    function ZCARD(const Key: string): Integer; virtual; abstract;
    function ZCOUNT(const Key: string; Min, Max: Integer): Integer; virtual; abstract;
    function ZINCRBY(const Key, Member: string; Increment: Single): Single;
      virtual; abstract;
    function ZRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; virtual; abstract;
    function ZRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray;
      virtual; abstract;
    function ZRANK(const Key, Member: string): Integer; virtual; abstract;
    function ZREM(const Key, Members: string): Integer; virtual; abstract;
    function ZREMRANGEBYRANK(const Key: string; Start, Stop: Integer): Integer;
      virtual; abstract;
    function ZREMRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray;
      virtual; abstract;
    function ZREVRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; virtual; abstract;
    function ZREVRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray;
      virtual; abstract;
    function ZREVRANK(const Key, Member: string): Integer; virtual; abstract;
    function ZSCORE(const Key, Member: string): string; virtual; abstract;
    function ZUNIONSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; virtual; abstract;
    function ZINTERSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; virtual; abstract;
    function ZSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; virtual; abstract;

   //-----------------------Pub/Sub-------------------------//
    function PSUBSCRIBE(const Patterns: string; Value: TStrings): Integer;
      virtual; abstract;
    function PUBLISH(const Channel, message: string): Integer; virtual; abstract;
    function PUBSUB(const Command, Arguments: string; Value: TStrings): Integer;
      virtual; abstract;
    function PUNSUBSCRIBE(const patterns: string): string; virtual; abstract;
    function SUBSCRIBE(const Channels: string; Value: TStrings): Integer;
      virtual; abstract;
    function UNSUBSCRIBE(const Channels: string; Value: TStrings): Integer;
      virtual; abstract;

    //---------------------Transaction-----------------------//
    function DISCARD: Boolean; virtual; abstract;
    function EXEC(Value: TStrings): Integer; virtual; abstract;
    function MULTI: Boolean; virtual; abstract;
    function UNWATCH: Boolean; virtual; abstract;
    function WATCH(const Keys: string): Boolean; virtual; abstract;

    //------------------------Script-------------------------//
    function EVAL(const Script, Keys, Arg: string; Value: TStrings): Integer;
      virtual; abstract;
    function EVALSHA(const Sha1, Keys, Arg: string): string; virtual; abstract;
    function SCRIPTEXISTS(const Scripts: string; Value: TStrings): Integer;
      virtual; abstract;
    function SCRIPTFLUSH: Boolean; virtual; abstract;
    function SCRIPTKILL: Boolean; virtual; abstract;
    function SCRIPTLOAD(const Script: string): string; virtual; abstract;

    //----------------------Connection-----------------------//
    function AUTH(const Password: string): Boolean; virtual; abstract;
    function ECHO(const Msg: string): string; virtual; abstract;
    function PING: Boolean; virtual; abstract;
    procedure QUIT; virtual; abstract;
    function SELECT(DB: Integer): Boolean; virtual; abstract;

    //------------------------Server-------------------------//
    function BGREWRITEAOF: string; virtual; abstract;
    function BGSAVE: string; virtual; abstract;
    function CLIENTGETNAME: string; virtual; abstract;
    function CLIENTKILL(const IP: string; Port: Word): Boolean; virtual; abstract;
    function CLIENTLIST(Value: TStrings): Integer; virtual; abstract;
    function CLIENTSETNAME(const Name: string): Boolean; virtual; abstract;
    function CONFIGGET(const Parameters: string): TCnRedisKeyValueArray; virtual;
      abstract;
    function CONFIGRESETSTAT: Boolean; virtual; abstract;
    function CONFIGREWRITE: Boolean; virtual; abstract;
    function CONFIGSET(const Parameter, Value: string): Boolean; virtual; abstract;
    function DBSIZE: Int64; virtual; abstract;
    function DEBUGOBJECT(const Key: string): string; virtual; abstract;
    procedure DEBUGSEGFAULT; virtual; abstract;
    function FLUSHALL: Boolean; virtual; abstract;
    function FLUSHDB: Boolean; virtual; abstract;
    function INFO(Section: TCnRedisInfoSection; Value: TStrings): Integer;
      virtual; abstract;
    function LASTSAVE: Int64; virtual; abstract;
    function MONITOR: Boolean; virtual; abstract;
    function PSYNC(const MASTER_RUN_ID: string; Offset: Integer): string;
      virtual; abstract;
    function SAVE: Boolean; virtual; abstract;
    function SHUTDOWN: string; virtual; abstract;
    function SLAVEOF(const Host: string; Port: Word): Boolean; virtual; abstract;
    function SLOWLOGLEN(const Parameter: string): Integer; virtual; abstract;
    function SLOWLOGGET(const Parameter: string): TCnRedisMultiBulk; virtual; abstract;
    function SLOWLOGRESET: Boolean; virtual; abstract;
    function SYNC: string; virtual; abstract;
    function TIME: TCnRedisKeyValue; virtual; abstract;
{$IFDEF IDE_EDITOR_SUPPORTS_FOLDING}
   {$ENDREGION}
{$ENDIF}
  end;

  TCnRedisClient = class(TCnRedisProtocol)
  public
{$IFDEF IDE_EDITOR_SUPPORTS_FOLDING}
    {$REGION '==========Redis Command=========='}
{$ENDIF}
    //--------------------------KEY----------------------------//
    function DEL(const Keys: string): Int64; override;
    function DUMP(const Key: string): string; override;//*
    function EXISTS(const Key: string): Boolean; override;
    function EXPIRE(const Key: string; Seconds: Int64): Boolean; override;
    function EXPIREAT(const Key: string; UnixTime: Int64): Boolean; override;
    function KEYS(const pattern: string; Value: TStrings): Integer; override;
    function MIGRATE(const Host: string; Port: Word; const Key: string; DestDB,
      Timeout: Cardinal; IsCopy, IsReplace: Boolean): Boolean; override;//*
    function MOVE(const Key: string; DB: Integer): Boolean; override;
    function OBJECTREFCOUNT(const key: string): Integer; override;
    function OBJECTENCODING(const key: string): string; override;
    function OBJECTIDLETIME(const key: string): Integer; override;
    function PERSIST(const Key: string): Boolean; override;
    function PEXPIRE(const Key: string; MilliSeconds: Int64): Boolean; override;
    function PEXPIREAT(const Key: string; UnixTime: Int64): Boolean; override;
    function TTL(const Key: string): Int64; override;
    function PTTL(const Key: string): Int64; override;
    function RANDOMKEY: string; override;
    function RENAME(const Key, NewKey: string): Boolean; override;
    function RENAMENX(const Key, NewKey: string): Boolean; override;
    function RESTORE(const Key: string; ttl: Cardinal; const SValue: string):
      Boolean; override;
    function SORT(const Key, Param: string; Value: TStrings): Integer; override;
    function _TYPE(const Key: string): TCnRedisDataType; override;
    function SCAN(cursor: Integer; MATCH: string; Count: Integer):
      TCnRedisMultiBulk; override;

    //-------------------------String---------------------------//
    function APPEND(const Key, Value: string): Integer; override;
    function BITCOUNT(const Key: string; Start: Integer = 0; Stop: Integer = 0):
      Integer; override;
    function BITOP(operation: TCnRedisOperation; const DestKey, Keys: string):
      Integer; override;
    function DECR(const Key: string): Int64; override;
    function DECRBY(const Key: string; Decrement: Int64): Int64; override;
    function GETRANGE(const Key: string; Start, Stop: Integer): string; override;
    function _SET(const Key, Value: string; EXSecond: Cardinal; Exist: Byte):
      Boolean; override;
    function GET(const Key: string): string; override;
    function GETBIT(const Key: string; offset: Integer): Integer; override;
    function GETSET(const Key, Value: string): string; override;
    function INCR(const Key: string): Int64; override;
    function INCRBY(const Key: string; Increment: Int64): Int64; override;
    function INCRBYFLOAT(const Key: string; Increment: Single): Single; override;
    function MGET(const Keys: string; Value: TStrings): Integer; override;
    function MSET(const KVs: string): Boolean; override;
    function MSETNX(const KVs: string): Boolean; override;
    function PSETEX(const Key, Value: string; MilliSeconds: Int64): Boolean; override;
    function SETBIT(const Key: string; Offset, Value: Integer): Integer; override;
    function SETEX(const Key, Value: string; Seconds: Int64): Boolean; override;
    function SETNX(const Key, Value: string): Boolean; override;
    function SETRANGE(const Key, value: string; Offset: Integer): Integer; override;
    function STRLEN(const Key: string): Integer; override;

    //-------------------------HASH---------------------------//
    function HDEL(const Key, Fields: string): Integer; override;
    function HEXISTS(const Key, Field: string): Boolean; override;
    function HGET(const Key, Field: string): string; override;
    function HGETALL(const Key: string; var Value: TCnRedisKeyValueArray):
      Integer; override;
    function HINCRBY(const Key, Field: string; Increment: Int64): Int64; override;
    function HINCRBYFLOAT(const Key, Field: string; Increment: Single): Single; override;
    function HKEYS(const Key: string; Value: TStrings): Integer; override;
    function HLEN(const Key: string): Integer; override;
    function HMGET(const Key, Fields: string; Value: TStrings): Integer; override;
    function HMSET(const Key, fvs: string): Boolean; override;
    function HSET(const Key, Field, Value: string): Boolean; override;
    function HSETNX(const Key, Field, Value: string): Boolean; override;
    function HVALS(const Key: string; Value: TStrings): Integer; override;
    function HSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; override;

    //-------------------------LIST---------------------------//
    function BLPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue; override;
    function BRPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue; override;
    function BRPOPLPUSH(const source, Destination: string; Timeout: Cardinal):
      TCnRedisKeyValue; override;
    function LINDEX(const Key: string; index: Integer): string; override;
    function LINSERT(const Key, Pivot, Value: string; IsBEFORE: Boolean):
      Integer; override;
    function LLEN(const Key: string): Integer; override;
    function LPOP(const Key: string): string; override;
    function LPUSH(const Key, values: string): Integer; override;
    function LPUSHX(const Key, Value: string): Integer; override;
    function LRANGE(const Key: string; Start, Stop: Integer; Value: TStrings):
      Integer; override;
    function LREM(const Key, Value: string; Count: Integer): Integer; override;
    function LSET(const Key, Value: string; Index: Integer): Boolean; override;
    function LTRIM(const Key: string; Start, Stop: Integer): Boolean; override;
    function RPOP(const Key: string): string; override;
    function RPOPLPUSH(const source, Destination: string): string; override;
    function RPUSH(const Key, values: string): Integer; override;
    function RPUSHX(const Key, Value: string): Integer; override;

    //-------------------------Set---------------------------//
    function SADD(const Key, Members: string): Integer; override;
    function SCARD(const Key: string): Integer; override;
    function SDIFF(const Keys: string; Value: TStrings): Integer; override;
    function SDIFFSTORE(const Keys, Destination: string): Integer; override;
    function SINTER(const Keys: string; Value: TStrings): Integer; override;
    function SINTERSTORE(const Destination, Keys: string): Integer; override;
    function SISMEMBER(const Key, Member: string): Boolean; override;
    function SMEMBERS(const Key: string; Value: TStrings): Integer; override;
    function SMOVE(const source, Destination, Member: string): Boolean; override;
    function SPOP(const Key: string): string; override;
    function SRANDMEMBER(const Key: string; Count: Integer; Value: TStrings):
      Integer; override;
    function SREM(const Key, Members: string): Integer; override;
    function SUNION(const Keys: string; Value: TStrings): Integer; override;
    function SUNIONSTORE(const Keys, Destination: string): Integer; override;
    function SSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; override;

    //----------------------SortedSet------------------------//
    function ZADD(const Key, ScoreMembers: string): Integer; override;
    function ZCARD(const Key: string): Integer; override;
    function ZCOUNT(const Key: string; Min, Max: Integer): Integer; override;
    function ZINCRBY(const Key, Member: string; Increment: Single): Single; override;
    function ZRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; override;
    function ZRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; override;
    function ZRANK(const Key, Member: string): Integer; override;
    function ZREM(const Key, Members: string): Integer; override;
    function ZREMRANGEBYRANK(const Key: string; Start, Stop: Integer): Integer; override;
    function ZREMRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; override;
    function ZREVRANGE(const Key: string; Start, Stop: Integer; IsWITHSCORES:
      Boolean): TCnRedisKeyValueArray; override;
    function ZREVRANGEBYSCORE(const Key: string; Min, Max: Integer; IsWITHSCORES:
      Boolean; offset: Integer = 0; count: Integer = 0): TCnRedisKeyValueArray; override;
    function ZREVRANK(const Key, Member: string): Integer; override;
    function ZSCORE(const Key, Member: string): string; override;
    function ZUNIONSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; override;
    function ZINTERSTORE(const Destination, Keys: string; KeyCount: Integer;
      WEIGHTS, AGGREGATE: string): Integer; override;
    function ZSCAN(const Key: string; Cursor: Integer; MATCH: string; Count:
      Integer): TCnRedisMultiBulk; override;

   //-----------------------Pub/Sub-------------------------//
    function PSUBSCRIBE(const Patterns: string; Value: TStrings): Integer; override; //*
    function PUBLISH(const Channel, message: string): Integer; override;
    function PUBSUB(const Command, Arguments: string; Value: TStrings): Integer;
      override; //*
    function PUNSUBSCRIBE(const patterns: string): string; override;   //*
    function SUBSCRIBE(const Channels: string; Value: TStrings): Integer; override;  //*
    function UNSUBSCRIBE(const Channels: string; Value: TStrings): Integer; override;//*

    //---------------------Transaction-----------------------//
    function DISCARD: Boolean; override;
    function EXEC(Value: TStrings): Integer; override;
    function MULTI: Boolean; override;
    function UNWATCH: Boolean; override;
    function WATCH(const Keys: string): Boolean; override;

    //------------------------Script-------------------------//
    function EVAL(const Script, Keys, Arg: string; Value: TStrings): Integer; override;
    function EVALSHA(const Sha1, Keys, Arg: string): string; override;
    function SCRIPTEXISTS(const Scripts: string; Value: TStrings): Integer; override;
    function SCRIPTFLUSH: Boolean; override;
    function SCRIPTKILL: Boolean; override;
    function SCRIPTLOAD(const Script: string): string; override;

    //----------------------Connection-----------------------//
    function AUTH(const Password: string): Boolean; override;
    function ECHO(const Msg: string): string; override;
    function PING: Boolean; override;
    procedure QUIT; override;
    function SELECT(DB: Integer): Boolean; override;

    //------------------------Server-------------------------//
    function BGREWRITEAOF: string; override;
    function BGSAVE: string; override;
    function CLIENTGETNAME: string; override;
    function CLIENTKILL(const IP: string; Port: Word): Boolean; override;
    function CLIENTLIST(Value: TStrings): Integer; override;
    function CLIENTSETNAME(const Name: string): Boolean; override;
    function CONFIGGET(const Parameters: string): TCnRedisKeyValueArray; override;
    function CONFIGRESETSTAT: Boolean; override;
    function CONFIGREWRITE: Boolean; override;
    function CONFIGSET(const Parameter, Value: string): Boolean; override;
    function DBSIZE: Int64; override;
    function DEBUGOBJECT(const Key: string): string; override;
    procedure DEBUGSEGFAULT; override;
    function FLUSHALL: Boolean; override;
    function FLUSHDB: Boolean; override;
    function INFO(Section: TCnRedisInfoSection; Value: TStrings): Integer; override;
    function LASTSAVE: Int64; override;
    function MONITOR: Boolean; override;
    function SAVE: Boolean; override;
    function SHUTDOWN: string; override;
    function SLAVEOF(const Host: string; Port: Word): Boolean; override;
    function SLOWLOGLEN(const Parameter: string): Integer; override;
    function SLOWLOGGET(const Parameter: string): TCnRedisMultiBulk; override;
    function SLOWLOGRESET: Boolean; override;
    function TIME: TCnRedisKeyValue; override;
{$IFDEF IDE_EDITOR_SUPPORTS_FOLDING}
   {$ENDREGION}
{$ENDIF}
  end;

// 从对象池里拿出一个 TCnRedisMultiBulkNode
function ObtainRedisMultiBulkNodeFromPool: TCnRedisMultiBulkNode;

// 将一个 TCnRedisMultiBulkNode 实例归还给对象池
procedure RecycleRedisMultiBulkNode(ANode: TCnRedisMultiBulkNode);

implementation

var
  FCnRedisMultiBulkNodePool: TObjectList = nil;

{ TCnRedisClientSocket }

function TCnRedisClientSocket.Connect: Boolean;
begin
  if FSocket.Active then
    FSocket.Active := False;
  FSocket.Host := RedisHost;
  FSocket.Port := RedisPort;
  FSocket.ClientType := ctBlocking;
  FSocket.Active := True;
  Result := FSocket.Active;
end;

constructor TCnRedisClientSocket.Create;
begin
  inherited;
  FSocket := TClientSocket.Create(nil);
  FSocket.OnDisconnect := OnDisconnect;
end;

destructor TCnRedisClientSocket.Destroy;
begin
  Disconnect;
  FreeAndNil(FSocket);
  inherited;
end;

procedure TCnRedisClientSocket.Disconnect;
begin
  inherited;
  if FSocket.Active then
    FSocket.Active := False;
end;

function TCnRedisClientSocket.GetConnecting: Boolean;
begin
  Result := FConnecting;
end;

function TCnRedisClientSocket.GetHost: string;
begin
  Result := FSocket.Host;
end;

function TCnRedisClientSocket.GetPassword: string;
begin
  Result := FPassword;
end;

function TCnRedisClientSocket.GetPort: Word;
begin
  Result := FSocket.Port;
end;

procedure TCnRedisClientSocket.OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  FConnecting := False;
end;

function TCnRedisClientSocket.RecvBuffer(Buffer: Pointer; Length: Cardinal): Integer;
begin
  Result := FSocket.Socket.ReceiveBuf(Buffer^, Length);
end;

function TCnRedisClientSocket.SendBuffer(Buffer: Pointer; Length: Cardinal): Integer;
begin
  Result := FSocket.Socket.SendBuf(Buffer^, Length);
end;

procedure TCnRedisClientSocket.SetConnecting(Value: Boolean);
begin
  FConnecting := Value;
end;

procedure TCnRedisClientSocket.SetHost(Value: string);
begin
  FSocket.Host := Value;
end;

procedure TCnRedisClientSocket.SetPassword(Value: string);
begin
  FPassword := Value;
end;

procedure TCnRedisClientSocket.SetPort(Value: Word);
begin
  FSocket.Port := Value;
end;

{ TCnRedisProtocol }

function TCnRedisProtocol.Connect: Boolean;
begin
  Result := False;
  if FInterfacedSocket.Connect then
  begin
    AUTH(FInterfacedSocket.Password);
    Result := PING;
  end;
end;

//constructor TCnRedisProtocol.Create(const SocketClassName: string);
//var
//  _Class: TPersistentClass;
//begin
//  if not Assigned( GetClass(SocketClassName)) then
//     raise Exception.Create(Format('无法创建[%s]类实例！',[SocketClassName]));
//  _Class:=  FindClass(SocketClassName);
//  if Assigned(_Class) then
//   begin
//      FInterfacedSocket:= TRedisAbstractSocketType(_Class).Create as IRedisSocket;
//      FInterfacedSocket.Password:='';
//      FInterfacedSocket.Connecting := False;
//      SetRecvBufferSize(cRedisDefaultBufferSize);
//   end;
//   exit;
//  FInterfacedSocket := CreateSocketOfClassName(SocketClassName);
//  FInterfacedSocket.Password := '';
//  FInterfacedSocket.Connecting := False;
//  SetRecvBufferSize(SCN_REDISD_EFAULT_BUFFER_SIZE);
//end;

constructor TCnRedisProtocol.Create(ASocketIntf: ICnRedisSocket);
begin
  if ASocketIntf = nil then
    FInterfacedSocket := TCnRedisClientSocket.Create
  else
    FInterfacedSocket := ASocketIntf;

  FInterfacedSocket.Password := '';
  FInterfacedSocket.Connecting := False;
  SetRecvBufferSize(SCN_REDISD_EFAULT_BUFFER_SIZE);
end;

function TCnRedisProtocol.CreateSocketOfClassName(const Name: string): ICnRedisSocket;
var
  _Class: TPersistentClass;
begin
  Result := nil;
  if not Assigned(GetClass(Name)) then
    raise Exception.Create(Format('未注册Socket类型:[%s]！', [Name]));
  _Class := FindClass(Name);
  if Assigned(_Class) then
  begin
    Result := TCnRedisAbstractSocketType(_Class).Create as ICnRedisSocket;
  end
  else
    raise Exception.Create(Format('创建[%s]实例失败！', [Name]));
end;

function TCnRedisProtocol.Deserialize(Response: Pointer; Length: Integer;
  RespNode: TCnRedisMultiBulkNode): Boolean;
var
  _Count: Integer;
  _Curr, _Pos: DWORD;
  I: Integer;
  _Value: AnsiString;
  CurNode: TCnRedisMultiBulkNode;

  function _ParseCount(p: DWORD; var Count: Integer): DWORD;
  var
    _Value: AnsiString;
  begin
    Count := 0;
    Result := 0;
    while PWord(p + Result)^ <> $0A0D do
      if Result < 4 then
        Inc(Result)
      else
        Exit;
    SetLength(_Value, Result - 1);
    CopyMemory(@_Value[1], Pointer(p + 1), Result - 1);
    Count := StrToInt(string(_Value));
    Inc(Result, 2);
    if Count < 0 then
      Count := 0;
  end;

begin
  Result := False;
  _Curr := 0;
  _Pos := 0;
  CurNode := RespNode;
  CurNode.ChangeMultiBulksSize(0);

  CurNode.Parent := nil;
  CurNode.CurrIndex := 0;
  if (Response = nil) or (Length < 2) then
    Exit;
  try
    while _Curr < DWORD(Length) do
    begin
      case PAnsiChar(DWORD(Response) + _Curr)^ of
        Char('*'):
          begin
            if CurNode.MultiBulkRefs.Count > 0 then
            begin
              CurNode := TCnRedisMultiBulkNode(CurNode.MultiBulkRefs[CurNode.CurrIndex]);
              CurNode.CurrIndex := 0;
            end;
            _Pos := _ParseCount(DWORD(Response) + _Curr, _Count);
            CurNode.ChangeMultiBulksSize(_Count);

            if _Count = 0 then
              CurNode.Value := '(empty)';

            for I := 0 to _Count - 1 do
            begin
              TCnRedisMultiBulkNode(CurNode.MultiBulkRefs[I]).Parent := CurNode;
              TCnRedisMultiBulkNode(CurNode.MultiBulkRefs[I]).CurrIndex := 0;
            end;
          end;

        Char('$'):
          begin
            if CurNode.MultiBulkRefs.Count = 0 then
            begin
              _Pos := _ParseCount(DWORD(Response) + _Curr, _Count);
              SetLength(_Value, _Count);
              CopyMemory(@_Value[1], Pointer(DWORD(Response) + _Curr + _Pos), _Count);
              CurNode.Value := string(_Value);
            end
            else
            begin
              if CurNode.CurrIndex > CurNode.MultiBulkRefs.Count - 1 then
              begin
                CurNode := CurNode.Parent;
                Inc(CurNode.FCurrIndex);
              end;
              _Pos := _ParseCount(DWORD(Response) + _Curr, _Count);
              SetLength(_Value, _Count);
              CopyMemory(@_Value[1], Pointer(DWORD(Response) + _Curr + _Pos), _Count);
              TCnRedisMultiBulkNode(CurNode.MultiBulkRefs[CurNode.CurrIndex]).Value := string(_Value);
              Inc(CurNode.FCurrIndex);
            end;
            Inc(_Pos, _Count + 2);
          end;

        Char('-'), Char('+'), Char(':'):
          begin
            CurNode.Value := string(PAnsiChar(DWORD(Response) + 1));
            SetLength(CurNode.FValue, Length - 3);  //除去最后/r/n 2字符
            Break;
          end;
      end;
      Inc(_Curr, _Pos);
    end;
    Result := True;
  except
  end;

end;

destructor TCnRedisProtocol.Destroy;
begin
  if Connecting then
    FInterfacedSocket.Disconnect;
  SetRecvBufferSize(0);
  FInterfacedSocket := nil;
  inherited;
end;

procedure TCnRedisProtocol.Disconnect;
begin
  if Connecting then
    QUIT;
  FInterfacedSocket.Connecting := False;
end;

function TCnRedisProtocol.GetConnecting: Boolean;
begin
  Result := FInterfacedSocket.Connecting;
end;

function TCnRedisProtocol.GetRedisDataTypeOfString(const Value: string): TCnRedisDataType;
var
  _pType: PTypeData;
  I: Integer;
begin
  Result := rdt_none;
  _pType := GetTypeData(TypeInfo(TCnRedisDataType));
  for I := _pType.MinValue to _pType.MaxValue do
  begin
    if GetEnumName(TypeInfo(TCnRedisDataType), I) = 'rdt_' + Value then
      Result := TCnRedisDataType(I);
  end;
end;

procedure TCnRedisProtocol.PipelineBegin;
begin
  FPipelineMode := True;
  FPipelineBuffer := '';
end;

function TCnRedisProtocol.PipelineEnd(Recv: TCnRedisMultiBulk): Boolean;
var
  Len: integer;
begin
  Result := False;
  FPipelineMode := False;
  FInterfacedSocket.SendBuffer(@FPipelineBuffer[1], Length(FPipelineBuffer));
  Len := FInterfacedSocket.RecvBuffer(FRecvBuffer, Length(FRecvBuffer));
  if (Len < 1) or (Len > Integer(Length(Self.FRecvBuffer))) then
    Exit;
  Result := Deserialize(FRecvBuffer, Len, Recv);
end;

function TCnRedisProtocol.SendAndReceive(const Send: string; Recv:
  TCnRedisMultiBulkNode; Force: Boolean): Boolean;
var
  _Send: AnsiString;
  Len: Integer;
begin
  Result := False;
  if (not Force) and (not Connecting) then
    FInterfacedSocket.Connecting := Connect;
  Len := Serialize(Send, _Send);
  if Len < 1 then
    Exit;
  if FPipelineMode and (not Force) then
  begin
    FPipelineBuffer := FPipelineBuffer + _Send;
    Exit;
  end;
  FInterfacedSocket.SendBuffer(@_Send[1], Len);
  Len := FInterfacedSocket.RecvBuffer(FRecvBuffer, Length(FRecvBuffer));
  if (Len < 1) or (Len > Integer(Length(Self.FRecvBuffer))) then
    Exit;
  Result := Deserialize(FRecvBuffer, Len, Recv);
end;

function TCnRedisProtocol.Serialize(const Value: string; var Serial: AnsiString): Integer;
var
  I: Integer;
  _ParamList: TStrings;
begin
  Result := 0;
  Serial := '';
  if Length(Value) <= 0 then
    Exit;
  _ParamList := TStringList.Create;
  try
    _ParamList.Delimiter := ' ';
    _ParamList.DelimitedText := Value;
    Serial := Ansistring('*' + IntToStr(_ParamList.Count) + SCN_REDIS_CRLF);
    for I := 0 to _ParamList.Count - 1 do
    begin
      Serial := Serial + '$' + Ansistring(IntToStr(Length(_ParamList.Strings[I]))
        + SCN_REDIS_CRLF + _ParamList.Strings[I] + SCN_REDIS_CRLF);
    end;
    Result := Length(Serial);
  finally
    FreeAndNil(_ParamList);
  end;
end;

procedure TCnRedisProtocol.SetRecvBufferSize(value: Cardinal);
begin
  if Connecting then
    Exit;
  if value = Cardinal(Length(FRecvBuffer)) then
    Exit;
  SetLength(FRecvBuffer, value);
end;

procedure TCnRedisProtocol.SetRedisServer(const Host: string; Port: Word; const
  Password: string);
begin
  FInterfacedSocket.RedisHost := Host;
  FInterfacedSocket.RedisPort := Port;
  FInterfacedSocket.Password := Password;
end;

{ TCnRedisClient }

function TCnRedisClient.BGREWRITEAOF: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BGREWRITEAOF', []), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BGSAVE: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BGSAVE', []), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BITCOUNT(const Key: string; Start: Integer; Stop:
  Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
  _Str: string;
begin
  Result := 0;
  if (Start = 0) and (Stop = 0) then
    _Str := Format('BITCOUNT %s ', [Key])
  else
    _Str := Format('BITCOUNT %s %d %d', [Key, Start, Stop]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Str, Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BITOP(operation: TCnRedisOperation; const DestKey, Keys:
  string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BITOP %s %s %s', [SCN_REDIS_OPERATION[Integer(operation)],
      DestKey, Keys]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BLPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Key := SCN_REDIS_EMPTY_VALUE;
  Result.Value := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BLPOP %s %d', [Keys, Timeout]), Reply) then
      if Reply.MultiBulkRefs.Count > 1 then
      begin
        Result.Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[0]).Value;
        Result.Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[1]).Value;
      end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BRPOP(const Keys: string; Timeout: Cardinal): TCnRedisKeyValue;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Key := SCN_REDIS_EMPTY_VALUE;
  Result.Value := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BRPOP %s %d', [Keys, Timeout]), Reply) then
      if Reply.MultiBulkRefs.Count > 1 then
      begin
        Result.Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[0]).Value;
        Result.Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[1]).Value;
      end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.BRPOPLPUSH(const source, Destination: string; Timeout:
  Cardinal): TCnRedisKeyValue;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Key := SCN_REDIS_EMPTY_VALUE;
  Result.Value := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('BRPOPLPUSH %s %d', [source, Destination, Timeout]),
      Reply) then
      if Reply.MultiBulkRefs.Count > 1 then
      begin
        Result.Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[0]).Value;
        Result.Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[1]).Value;
      end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CLIENTGETNAME: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('CLIENT GETNAME', []), Reply) then
    Result := Reply.Value;
end;

function TCnRedisClient.CLIENTKILL(const IP: string; Port: Word): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CLIENT KILL %s:%d', [IP, Port]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CLIENTLIST(Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CLIENT LIST', []), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CLIENTSETNAME(const Name: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('CLIENT SETNAME %s ', [Name]), Reply) then
    Result := UpperCase(Reply.Value) = 'OK';
end;

function TCnRedisClient.CONFIGGET(const Parameters: string): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
begin
  SetLength(Result, 0);
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CONFIG GET %s', [Parameters]), Reply) then
    begin
      SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
      for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
      begin
        Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
        Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CONFIGRESETSTAT: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('CONFIG RESETSTAT ', []), Reply) then
    Result := UpperCase(Reply.Value) = 'OK';
end;

function TCnRedisClient.CONFIGREWRITE: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CONFIG REWRITE ', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.CONFIGSET(const Parameter, Value: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('CONFIGSET %s %s', [Parameter, Value]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DISCARD: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DISCARD', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DUMP(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DUMP %s', [Key]), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ECHO(const Msg: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ECHO %s', [Msg]), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EVAL(const Script, Keys, Arg: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
  _Count: Integer;
  _Tlist: TStrings;
begin
  Result := 0;
  _Tlist := TStringList.Create;
  try
    _Tlist.DelimitedText := Keys;
    _Tlist.Delimiter := ' ';
    _Count := _Tlist.Count;
    _Tlist.DelimitedText := Arg;
    _Tlist.Delimiter := ' ';
    if _Count <> _Tlist.Count then
      Exit;
  finally
    FreeAndNil(_Tlist);
  end;

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EVAL %s %d %s %s', [Script, _Count, Keys, Arg]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EVALSHA(const Sha1, Keys, Arg: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EVALSHA %s %s %s', [Sha1, Keys, Arg]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EXEC(Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EXEC', []), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EXISTS(const Key: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EXISTS %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EXPIRE(const Key: string; Seconds: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EXPIRE %s %d', [Key, Seconds]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.EXPIREAT(const Key: string; UnixTime: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('EXPIREAT %s %d', [Key, UnixTime]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.FLUSHALL: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('FLUSHALL', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.FLUSHDB: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('FLUSHDB', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.GET(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '(nil)';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('Get %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.GETBIT(const Key: string; offset: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('GETBIT %s %d', [Key, offset]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.GETRANGE(const Key: string; Start, Stop: Integer): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('GETRANGE %s %d %d', [Key, Start, Stop]), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.GETSET(const Key, Value: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('GETSET %s %s', [Key, Value]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HDEL(const Key, Fields: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HDEL %s %s', [Key, Fields]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HEXISTS(const Key, Field: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HEXISTS %s %s', [Key, Field]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HGET(const Key, Field: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HEXISTS %s %s', [Key, Field]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HGETALL(const Key: string; var Value:
  TCnRedisKeyValueArray): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  SetLength(Value, 0);
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HGETALL %s', [Key]), Reply) then
    begin
      SetLength(Value, (Reply.MultiBulkRefs.Count) div 2);
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
      begin
        Value[Result].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[Result * 2]).Value;
        Value[Result].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[Result * 2 + 1]).Value;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HINCRBY(const Key, Field: string; Increment: Int64): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HINCRBY %s %s %d', [Key, Field, Increment]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HINCRBYFLOAT(const Key, Field: string; Increment: Single): Single;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HINCRBYFLOAT %s %s %8.4f', [Key, Field, Increment]),
      Reply) then
      if Reply.Value <> '' then
        Result := StrToFloat(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HKEYS(const Key: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HKEYS %s', [Key]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HLEN(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HLEN %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HMGET(const Key, Fields: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HMGET %s %s', [Key, Fields]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HMSET(const Key, fvs: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HMSET %s', [Key, fvs]), Reply) then
      if Reply.Value <> '' then
        Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HSCAN(const Key: string; Cursor: Integer; MATCH: string;
  Count: Integer): TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result.Value := '(nil)';
  _Cmd := Format('HSCAN %s %d', [Key, Cursor]);
  if MATCH <> '' then
    _Cmd := Format('%s MATCH %s', [_Cmd, MATCH]);
  if Count > 0 then
    _Cmd := Format('%s COUNT %d', [_Cmd, Count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      Reply.Value := '(Multi-Bulk)';
      Result := Reply;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HSET(const Key, Field, Value: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HSET %s %s %s', [Key, Field, Value]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HSETNX(const Key, Field, Value: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HSETNX %s %s %s', [Key, Field, Value]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.HVALS(const Key: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('HVALS %s', [Key]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.INCR(const Key: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('INCR %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.INCRBY(const Key: string; Increment: Int64): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('INCRBY %s %d', [Key, Increment]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.INCRBYFLOAT(const Key: string; Increment: Single): Single;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('INCRBY %s %8.4f', [Key, Increment]), Reply) then
      if Reply.Value <> '' then
        Result := StrToFloat(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.INFO(Section: TCnRedisInfoSection; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('INFO %s', [SCN_REDIS_INFO_SECTION_NAME[Integer(Section)]]),
      Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.Keys(const pattern: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('KEYS %s', [pattern]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LASTSAVE: Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LASTSAVE', []), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LINDEX(const Key: string; index: Integer): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LINDEX %s %d', [Key, index]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LINSERT(const Key, Pivot, Value: string; IsBEFORE:
  Boolean): Integer;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := 0;
  if IsBEFORE then
    _Cmd := Format('LINSERT %s BEFORE', [Key])
  else
    _Cmd := Format('LINSERT %s AFTER', [Key]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('%s %s %s', [_Cmd, Pivot, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LLEN(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LLEN %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LPOP(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LPOP %s', [Key]), Reply) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LPUSH(const Key, values: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LPUSH %s %s', [Key, values]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LPUSHX(const Key, Value: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LPUSHX %s %s', [Key, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LRANGE(const Key: string; Start, Stop: Integer; Value:
  TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LRANGE %s %d %d', [Key, Start, Stop]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LREM(const Key, Value: string; Count: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LREM %s %s %d', [Key, Value, Count]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LSET(const Key, Value: string; Index: Integer): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LSET %s %s %d', [Key, Value, Index]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.LTRIM(const Key: string; Start, Stop: Integer): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('LTRIM %s %d %d', [Key, Start, Stop]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MGET(const Keys: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MGET %s', [Keys]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MIGRATE(const Host: string; Port: Word; const Key:
  string; DestDB, Timeout: Cardinal; IsCopy, IsReplace: Boolean): Boolean;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := False;
  _Cmd := Format('MIGRATE %s %d %s %d %d ', [Host, Port, Key, DestDB, Timeout]);
  if IsCopy then
    _Cmd := Format('%s COPY', [_Cmd]);
  if IsReplace then
    _Cmd := Format('%s REPLACE', [_Cmd]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MONITOR: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MONITOR', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MOVE(const Key: string; DB: Integer): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MOVE %s %d', [Key, DB]), Reply) then
      Result := UpperCase(Reply.Value) = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MSET(const KVs: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    Result := SendAndReceive(Format('MSET %s', [KVs]), Reply);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MSETNX(const KVs: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MSETNX %s', [KVs]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.MULTI: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('MULTI', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PERSIST(const Key: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PERSIST %s', [Key]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PEXPIRE(const Key: string; MilliSeconds: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PEXPIRE %s %d', [Key, MilliSeconds]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PEXPIREAT(const Key: string; UnixTime: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PEXPIREAT %s %d', [Key, UnixTime]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PING: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive('PING', Reply, True) then
      Result := UpperCase(Reply.Value) = 'PONG';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.APPEND(const Key, Value: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('APPEND %s %s', [Key, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.OBJECTENCODING(const key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('OBJECT ENCODING %s', [key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.OBJECTIDLETIME(const key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('OBJECT IDLETIME %s', [key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.OBJECTREFCOUNT(const key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('OBJECT REFCOUNT %s', [key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PSETEX(const Key, Value: string; MilliSeconds: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PSETEX %s %s %d', [Key, Value, MilliSeconds]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PSUBSCRIBE(const Patterns: string; Value: TStrings): Integer;
begin
  Result := 0;
end;

function TCnRedisClient.PTTL(const Key: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := -3;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PTTL %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PUBLISH(const Channel, message: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('PUBLISH %s %s', [Channel, message]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.PUBSUB(const Command, Arguments: string; Value: TStrings):
  Integer;
begin

end;

function TCnRedisClient.PUNSUBSCRIBE(const patterns: string): string;
begin

end;

function TCnRedisClient.AUTH(const Password: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('AUTH %s', [Password]), Reply, True) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

procedure TCnRedisClient.QUIT;
var
  Reply: TCnRedisMultiBulk;
begin
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    SendAndReceive('QUIT', Reply, True);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RANDOMKEY: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RANDOMKEY', []), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RENAME(const Key, NewKey: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RENAME %s %s', [Key, NewKey]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RENAMENX(const Key, NewKey: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RENAMENX %s %s', [Key, NewKey]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RESTORE(const Key: string; ttl: Cardinal; const SValue:
  string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RESTORE %s %d %s', [Key, ttl, SValue]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RPOP(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RPOP %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RPOPLPUSH(const source, Destination: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RPOPLPUSH %s %s', [source, Destination]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RPUSH(const Key, values: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RPUSH %s %s', [Key, values]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.RPUSHX(const Key, Value: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('RPUSHX %s %s', [Key, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient._TYPE(const Key: string): TCnRedisDataType;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := rdt_none;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('TYPE %s', [Key]), Reply) then
      Result := GetRedisDataTypeOfString(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SADD(const Key, Members: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SADD %s %s', [Key, Members]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SAVE: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SAVE', []), Reply, False) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCAN(cursor: Integer; MATCH: string; Count: Integer):
  TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  _Cmd := Format('SCAN %d', [cursor]);
  if MATCH <> '' then
    _Cmd := Format('%s MATCH %s', [_Cmd, MATCH]);
  if Count > 0 then
    _Cmd := Format('%s COUNT %d', [_Cmd, Count]);

  Result := ObtainRedisMultiBulkNodeFromPool;
  Result.Value := '(nil)';
  if SendAndReceive(_Cmd, Result) then
    Result.Value := '(Multi-Bulk)';
end;

function TCnRedisClient.SCARD(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCARD %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCRIPTEXISTS(const Scripts: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCRIPT EXISTS %s', [Scripts]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCRIPTFLUSH: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCRIPT FLUSH', []), Reply, False) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCRIPTKILL: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCRIPT KILL', []), Reply, False) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SCRIPTLOAD(const Script: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SCRIPT LOAD %s', [Script]), Reply, False) then
      Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SDIFF(const Keys: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SDIFF %s', [Keys]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SDIFFSTORE(const Keys, Destination: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SDIFFSTORE %s', [Keys]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SELECT(DB: Integer): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SELECT %d', [DB]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient._SET(const Key, Value: string; EXSecond: Cardinal; Exist:
  Byte): Boolean;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := False;
  case Exist of
    0:
      begin
        if EXSecond = 0 then
          _Cmd := Format('SET %s "%s"', [Key, Value])
        else
          _Cmd := Format('SET %s "%s" EX %d', [Key, Value, EXSecond]);
      end;
    1:
      begin
        if EXSecond = 0 then
          _Cmd := Format('SET %s "%s" XX', [Key, Value])
        else
          _Cmd := Format('SET %s "%s" EX %d XX', [Key, Value, EXSecond]);
      end;
    2:
      begin
        if EXSecond = 0 then
          _Cmd := Format('SET %s "%s" NX', [Key, Value])
        else
          _Cmd := Format('SET %s "%s" EX %d NX', [Key, Value, EXSecond]);
      end;
  end;

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DBSIZE: Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DBSIZE', []), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DEBUGOBJECT(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DEBUG OBJECT %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

procedure TCnRedisClient.DEBUGSEGFAULT;
var
  Reply: TCnRedisMultiBulk;
begin
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    SendAndReceive(Format('DEBUG SEGFAULT', []), Reply);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DECR(const Key: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DECR %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DECRBY(const Key: string; Decrement: Int64): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DECR %s %d', [Key, Decrement]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SETBIT(const Key: string; Offset, Value: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SETBIT %s %d %d', [Key, Offset, Value]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SETEX(const Key, Value: string; Seconds: Int64): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SETEX %s %s %d', [Key, Value, Seconds]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SETNX(const Key, Value: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SETNX %s %s', [Key, Value]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SETRANGE(const Key, value: string; Offset: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SETRANGE %s %d %d', [Key, Offset, value]), Reply) then
      if Reply.value <> '' then
        Result := StrToInt(Reply.value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SHUTDOWN: string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SHUTDOWN', []), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SINTER(const Keys: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SINTER %s', [Keys]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SINTERSTORE(const Destination, Keys: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SINTERSTORE %s %s', [Destination, Keys]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SISMEMBER(const Key, Member: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SISMEMBER %s %s', [Key, Member]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SLAVEOF(const Host: string; Port: Word): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SLAVEOF %s %d', [Host, Port]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SLOWLOGGET(const Parameter: string): TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Value := '(nil)';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SLOWLOGGET %s', [Parameter]), Reply) then
    begin
      Reply.Value := '(Multi-Bulk)';
      Result := Reply;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SLOWLOGLEN(const Parameter: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SLOWLOG LEN %s', [Parameter]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SLOWLOGRESET: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SLOWLOGRESET', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SMEMBERS(const Key: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SMEMBERS %s', [Key]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SMOVE(const source, Destination, Member: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SMOVE %s %s %s', [source, Destination, Member]), Reply) then
      Result := Reply.Value = '1';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SORT(const Key, Param: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Value.Clear;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SORT %s %s', [Key, Param]), Reply) then
    begin
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value)
      else
      begin
        for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SPOP(const Key: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SPOP %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := Reply.Value;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SRANDMEMBER(const Key: string; Count: Integer; Value:
  TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result := 0;
  if (Count = 0) or (Count = 1) then
    _Cmd := Format('SRANDMEMBER %s', [Key])
  else
    Format('SRANDMEMBER %s %d', [Key, Count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SREM(const Key, Members: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SREM %s %s', [Key, Members]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SSCAN(const Key: string; Cursor: Integer; MATCH: string;
  Count: Integer): TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result.Value := '(nil)';
  _Cmd := Format('SSCAN %s %d', [Key, Cursor]);
  if MATCH <> '' then
    _Cmd := Format('%s MATCH %s', [_Cmd, MATCH]);
  if Count > 0 then
    _Cmd := Format('%s COUNT %d', [_Cmd, Count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      Reply.Value := '(Multi-Bulk)';
      Result := Reply;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.STRLEN(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('STRLEN %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SUBSCRIBE(const Channels: string; Value: TStrings): Integer;
begin
  Result := -1;
end;

function TCnRedisClient.SUNION(const Keys: string; Value: TStrings): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SUNION %s', [Keys]), Reply) then
      for Result := 0 to Reply.MultiBulkRefs.Count - 1 do
        if TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value = '' then
          Value.Add(SCN_REDIS_EMPTY_VALUE)
        else
          Value.Add(TCnRedisMultiBulk(Reply.MultiBulkRefs[Result]).Value);
    if Result = 0 then
      Value.Add(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.SUNIONSTORE(const Keys, Destination: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('SUNIONSTORE %s %s', [Keys, Destination]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.TIME: TCnRedisKeyValue;
var
  Reply: TCnRedisMultiBulk;
begin
  Result.Key := SCN_REDIS_EMPTY_VALUE;
  Result.Value := SCN_REDIS_EMPTY_VALUE;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('TIME', []), Reply) then
      if Reply.MultiBulkRefs.Count > 1 then
      begin
        Result.Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[0]).Value;
        Result.Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[1]).Value;
      end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ttl(const Key: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := -3;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('TTL %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.UNSUBSCRIBE(const Channels: string; Value: TStrings): Integer;
begin
  Result := -1;
end;

function TCnRedisClient.UNWATCH: Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('UNWATCH', []), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.WATCH(const Keys: string): Boolean;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := False;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('WATCH %s', [Keys]), Reply) then
      Result := UpperCase(Reply.Value) = 'OK';
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZADD(const Key, ScoreMembers: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZADD %s %s', [Key, ScoreMembers]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZCARD(const Key: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZCARD %s', [Key]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZCOUNT(const Key: string; Min, Max: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZCOUNT %s %d %d', [Key, Min, Max]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZINCRBY(const Key, Member: string; Increment: Single): Single;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZINCRBY %s %s %8.4f', [Key, Member, Increment]), Reply) then
      if Reply.Value <> '' then
        Result := StrToFloat(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZINTERSTORE(const Destination, Keys: string; KeyCount:
  Integer; WEIGHTS, AGGREGATE: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZINTERSTORE %s %s %d %s %s', [Destination, Keys,
      KeyCount, WEIGHTS, AGGREGATE]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZRANGE(const Key: string; Start, Stop: Integer;
  IsWITHSCORES: Boolean): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZRANGE %s %d %d', [Key, Start, Stop]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
      for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
      begin
        Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
        Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZRANGEBYSCORE(const Key: string; Min, Max: Integer;
  IsWITHSCORES: Boolean; offset, count: Integer): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZRANGEBYSCORE %s %d %d', [Key, Min, Max]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);
  if count <> 0 then
    _Cmd := Format('%s LIMIT %d %d', [offset, count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      if IsWITHSCORES then
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
        for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
          Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
        end;
      end
      else
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count));
        for I := 0 to Reply.MultiBulkRefs.Count - 1 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I]).Value;
          Result[I].Value := '';
        end;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZRANK(const Key, Member: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('ZRANK %s %s', [Key, Member]), Reply) then
    if Reply.Value <> '' then
      Result := StrToInt(Reply.Value);
end;

function TCnRedisClient.ZREM(const Key, Members: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZREM %s %s', [Key, Members]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREMRANGEBYRANK(const Key: string; Start, Stop: Integer): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZREMRANGEBYRANK %s %d %d', [Key, Start, Stop]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREMRANGEBYSCORE(const Key: string; Min, Max: Integer;
  IsWITHSCORES: Boolean; offset, count: Integer): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZREMRANGEBYSCORE %s %d %d', [Key, Min, Max]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);
  if count <> 0 then
    _Cmd := Format('%s LIMIT %d %d', [offset, count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      if IsWITHSCORES then
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
        for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
          Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
        end;
      end
      else
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count));
        for I := 0 to Reply.MultiBulkRefs.Count - 1 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I]).Value;
          Result[I].Value := '';
        end;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREVRANGE(const Key: string; Start, Stop: Integer;
  IsWITHSCORES: Boolean): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZREVRANGE %s %d %d', [Key, Start, Stop]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
      for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
      begin
        Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
        Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREVRANGEBYSCORE(const Key: string; Min, Max: Integer;
  IsWITHSCORES: Boolean; offset, count: Integer): TCnRedisKeyValueArray;
var
  Reply: TCnRedisMultiBulk;
  I: Integer;
  _Cmd: string;
begin
  SetLength(Result, 0);
  _Cmd := Format('ZREVRANGEBYSCORE %s %d %d', [Key, Min, Max]);
  if IsWITHSCORES then
    _Cmd := Format('%s WITHSCORES', [_Cmd]);
  if count <> 0 then
    _Cmd := Format('%s LIMIT %d %d', [offset, count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      if IsWITHSCORES then
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count) div 2);
        for I := 0 to Reply.MultiBulkRefs.Count - 1 div 2 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2]).Value;
          Result[I].Value := TCnRedisMultiBulk(Reply.MultiBulkRefs[I * 2 + 1]).Value;
        end;
      end
      else
      begin
        SetLength(Result, (Reply.MultiBulkRefs.Count));
        for I := 0 to Reply.MultiBulkRefs.Count - 1 do
        begin
          Result[I].Key := TCnRedisMultiBulk(Reply.MultiBulkRefs[I]).Value;
          Result[I].Value := '';
        end;
      end;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZREVRANK(const Key, Member: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZREVRANK %s %s', [Key, Member]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZSCAN(const Key: string; Cursor: Integer; MATCH: string;
  Count: Integer): TCnRedisMultiBulk;
var
  Reply: TCnRedisMultiBulk;
  _Cmd: string;
begin
  Result.Value := '(nil)';
  _Cmd := Format('ZSCAN %s %d', [Key, Cursor]);
  if MATCH <> '' then
    _Cmd := Format('%s MATCH %s', [_Cmd, MATCH]);
  if Count > 0 then
    _Cmd := Format('%s COUNT %d', [_Cmd, Count]);

  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(_Cmd, Reply) then
    begin
      Reply.Value := '(Multi-Bulk)';
      Result := Reply;
    end;
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.ZSCORE(const Key, Member: string): string;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := '0';
  Reply := ObtainRedisMultiBulkNodeFromPool;
  if SendAndReceive(Format('ZSCORE %s %s', [Key, Member]), Reply) then
    if Reply.Value <> '' then
      Result := Reply.Value;
end;

function TCnRedisClient.ZUNIONSTORE(const Destination, Keys: string; KeyCount:
  Integer; WEIGHTS, AGGREGATE: string): Integer;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('ZUNIONSTORE %s %s %d %s %s', [Destination, Keys,
      KeyCount, WEIGHTS, AGGREGATE]), Reply) then
      if Reply.Value <> '' then
        Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function TCnRedisClient.DEL(const Keys: string): Int64;
var
  Reply: TCnRedisMultiBulk;
begin
  Result := 0;
  Reply := ObtainRedisMultiBulkNodeFromPool;
  try
    if SendAndReceive(Format('DEL %s', [Keys]), Reply) then
      Result := StrToInt(Reply.Value);
  finally
    RecycleRedisMultiBulkNode(Reply);
  end;
end;

function ObtainRedisMultiBulkNodeFromPool: TCnRedisMultiBulkNode;
begin
  if (FCnRedisMultiBulkNodePool = nil) or (FCnRedisMultiBulkNodePool.Count = 0) then
  begin
    Result := TCnRedisMultiBulkNode.Create;
  end
  else
  begin
    Result := TCnRedisMultiBulkNode(FCnRedisMultiBulkNodePool.Items[FCnRedisMultiBulkNodePool.Count - 1]);
    FCnRedisMultiBulkNodePool.Delete(FCnRedisMultiBulkNodePool.Count - 1);
  end;
end;

procedure RecycleRedisMultiBulkNode(ANode: TCnRedisMultiBulkNode);
begin
  if ANode <> nil then
  begin
    if FCnRedisMultiBulkNodePool = nil then
      FCnRedisMultiBulkNodePool := TObjectList.Create(False);
    FCnRedisMultiBulkNodePool.Add(ANode);
  end;
end;

procedure FreeRedisMultiBulkNodePool;
var
  I: Integer;
begin
  if FCnRedisMultiBulkNodePool = nil then
    Exit;

  for I := 0 to FCnRedisMultiBulkNodePool.Count - 1 do
    TCnRedisMultiBulkNode(FCnRedisMultiBulkNodePool[I]).Free;
  FreeAndNil(FCnRedisMultiBulkNodePool);
end;

{ TCnRedisMultiBulkNode }

procedure TCnRedisMultiBulkNode.ChangeMultiBulksSize(NewCount: Integer);
var
  I: Integer;
begin
  if NewCount <= 0 then
  begin
    for I := 0 to FMultiBulkRefs.Count - 1 do
      RecycleRedisMultiBulkNode(TCnRedisMultiBulkNode(FMultiBulkRefs[I]));
    FMultiBulkRefs.Clear;
  end
  else if NewCount > FMultiBulkRefs.Count then
  begin
    for I := 0 to NewCount - FMultiBulkRefs.Count do
      FMultiBulkRefs.Add(ObtainRedisMultiBulkNodeFromPool);
  end
  else if NewCount < FMultiBulkRefs.Count then
  begin
    for I := FMultiBulkRefs.Count - 1 downto NewCount - 1 do
    begin
      RecycleRedisMultiBulkNode(TCnRedisMultiBulkNode(FMultiBulkRefs[I]));
      FMultiBulkRefs.Delete(I);
    end;
  end;
end;

constructor TCnRedisMultiBulkNode.Create;
begin
  FMultiBulkRefs := TObjectList.Create;
end;

destructor TCnRedisMultiBulkNode.Destroy;
var
  I: Integer;
begin
  for I := 0 to FMultiBulkRefs.Count - 1 do
    RecycleRedisMultiBulkNode(TCnRedisMultiBulkNode(FMultiBulkRefs[I]));
  FMultiBulkRefs.Free;
  inherited;
end;

initialization

finalization
  FreeRedisMultiBulkNodePool;

end.

