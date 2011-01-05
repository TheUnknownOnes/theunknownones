//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uWPuttyCDAdds;

interface

const
  //* Protocol back ends. (cfg.protocol) */
  PROT_RAW = 0;
  PROT_TELNET = 1;
  PROT_RLOGIN = 2;
  PROT_SSH = 3;
  PROT_SERIAL = 4;

  //Proxy types.
  PROXY_NONE = 0;
  PROXY_SOCKS4 = 1;
  PROXY_SOCKS5 = 2;
  PROXY_HTTP = 3;
  PROXY_TELNET = 4;
  PROXY_CMD = 5;

  {
  * Several different bits of the PuTTY configuration seem to be
  * three-way settings whose values are `always yes', `always
  * no', and `decide by some more complex automated means'. This
  * is true of line discipline options (local echo and line
  * editing), proxy DNS, Close On Exit, and SSH server bug
  * workarounds. Accordingly I supply a single enum here to deal
  * with them all.
  }
  FORCE_ON = 0;
  FORCE_OFF = 1;
  AUTO = 2;

  //X11 auth mechanisms we know about.
  X11_NO_AUTH = 0;
  X11_MIT = 1;
  X11_XDM = 2;
  X11_NAUTHS = 3;

  {
  * Network address types. Used for specifying choice of IPv4/v6
  * in config; also used in proxy.c to indicate whether a given
  * host name has already been resolved or will be resolved at
  * the proxy end.
  *}
  ADDRTYPE_UNSPEC = 0;
  ADDRTYPE_IPV4 = 1;
  ADDRTYPE_IPV6 = 2;
  ADDRTYPE_NAME = 3;


implementation

end.