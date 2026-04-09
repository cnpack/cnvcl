# CNT - CnPack NetTool

**CNT** (CnPack NetTool) 是一个类似于 NetCat (nc) 的命令行网络工具，使用 Delphi/FPC 开发，完整代码位于 cnvcl/Example/Linux/Cnt。

## 功能特性

### 连接模式
- **TCP 客户端模式**: 连接到远程主机 `cnt host port`
- **TCP 服务端模式**: 监听本地端口 `cnt -l -p port`
- **UDP 客户端/服务端模式**: 使用 `-u` 选项

### 数据处理
- **标准输入/输出**: 数据可以从 stdin 到网络、从网络到 stdout
- **HexDump 模式**: `-x` 选项以十六进制显示收发数据
- **静默模式**: `-q seconds` 接收完毕后延迟退出

### 连接控制
- **超时设置**: `-w seconds` 等待超时
- **空闲间隔**: `-i seconds` 空闲等待间隔
- **Keep-Alive**: `-K` 启用 SO_KEEPALIVE

### 高级功能
- **执行外部程序**: `-e cmd` 连接建立后执行命令（危险功能）
- **Shell 执行**: `-c cmd` 通过 shell 执行
- **端口扫描**: `-z` 零字节数据发送模式
- **广播**: `-b` 允许 UDP 广播

## 跨平台支持

本工具支持以下编译器和操作系统：

| 编译器 | 操作系统 | 状态 |
|--------|----------|------|
| FPC 3.0+ | Linux | ✅ 支持 |
| FPC 3.0+ | macOS | ✅ 支持 |
| FPC 3.0+ | Windows | ✅ 支持 |
| Delphi 5+ | Windows | ✅ 支持 |
| Delphi XE2+ | Linux/macOS | ✅ 支持 |

### 跨平台宏定义

代码中使用以下条件编译宏来区分平台：

```pascal
{$IFDEF FPC}         // Free Pascal 编译器
{$IFDEF MSWINDOWS}   // Windows 平台
{$IFDEF LINUX}       // Linux 平台
{$IFDEF DARWIN}      // macOS 平台 (FPC)
{$IFDEF UNIX}        // Unix/类Unix平台
```

## 编译

### 前置条件
- Free Pascal Compiler (FPC) 3.0+ (推荐)
- 或者 Delphi 5+

### FPC 编译方法 (Linux/macOS/Windows)

```bash
cd Example/Linux/Cnt
fpc -Sh cnt.lpr
```

或者在 Lazarus IDE 中打开 `cnt.lpi` 文件。

### Delphi 编译方法 (Windows)

1. 打开 Delphi IDE
2. 打开 `cnt.dpr` 文件
3. 编译运行

### 依赖

需要将 CnPack 路径添加到搜索路径：

```
Source/Common
Source/NetComm
```

## 使用示例

### 基本连接
```bash
# 连接到远程主机
cnt example.com 80

# 监听本地端口
cnt -l -p 8080

# UDP 模式
cnt -u example.com 53
```

### 文件传输
```bash
# 发送文件（接收端）
cnt -l -p 1234 > received_file

# 发送文件（发送端）
cat file_to_send | cnt target_host 1234
```

### 端口扫描
```bash
# 扫描单个端口
cnt -z example.com 80

# 扫描端口范围 (1-1024)
cnt -z example.com 1024
```

### 执行命令（Shell）
```bash
# 服务端：绑定 Bash 到端口
cnt -l -e /bin/bash -p 1234

# 客户端：连接上去获得 Shell
cnt target_host 1234
```

### 十六进制调试
```bash
# 显示收发数据的 HexDump
cnt -x host 80
```

## 与 NetCat (nc) 的对比

| 选项 | nc | cnt | 说明 |
|------|-----|-----|------|
| 帮助 | -h | -h | 显示帮助 |
| 监听 | -l | -l | 监听模式 |
| UDP | -u | -u | UDP 模式 |
| 端口 | -p | -p | 指定端口 |
| 执行 | -e | -e | 执行命令 |
| 超时 | -w | -w | 等待超时 |
| HexDump | -x | -x | 十六进制输出 |
| 端口扫描 | -z | -z | 零模式扫描 |
| 广播 | -b | -b | UDP 广播 |

## 项目结构

```
Example/Linux/Cnt/
├── cnt.dpr          # 主程序入口 (Delphi)
├── cnt.lpr          # 主程序入口 (FPC/Lazarus)
├── cnt.lpi          # Lazarus 项目文件
├── cnt.dof          # Delphi 项目选项
├── cnt.cfg          # FPC 配置文件
├── CntCmdLine.pas   # 命令行参数解析
├── CntConsts.pas    # 常量定义
├── CntDualComm.pas  # 双向命令行工具
├── CntUtils.pas     # 通用工具（HexDump等）
├── CntClient.pas    # TCP/UDP 客户端
├── CntServer.pas    # TCP/UDP 服务端
├── CntTunnel.pas    # 隧道/执行模式
├── CntScan.pas      # 端口扫描
```

## TODO

- [ ] TLS/SSL 支持
- [ ] IPv6 支持
- [ ] 完整 -e exec 实现 (当前仅 FPC/Unix)
- [ ] 多人聊天模式 (类似 ncat --chat)
- [ ] 管道传输优化
- [ ] Delphi 跨平台服务端实现

## 依赖

本项目利用了 CnPack VCL Components 的网络库：

- `Source/Common/CnPack.inc` - 公共编译指令
- `Source/NetComm/CnSocket.pas` - 跨平台 Socket 封装

## 许可证

本项目遵循 CnPack 许可协议第二版。

## 作者

CnPack 开发组

## 参考

- [NetCat 官方文档](http://nc110.sourceforge.net/)
- [OpenBSD nc](https://man.openbsd.org/nc.1)