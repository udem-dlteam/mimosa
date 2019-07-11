// file: "net.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 28 nov 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "net.h"
#include "rtlib.h"
#include "term.h"
#include "time.h"
#include "thread.h"

extern "C"
{
#include "etherboot.h"
#include "nic.h"
};

//-----------------------------------------------------------------------------

void show_packet()
{
  int q, len;

  len = (int)nic.packetlen;
  if (len >25) len = 25; 
  //if (nic.packet[0] != (char)0xff){
    for (q=0; q<len; q++)
      printf ("%hhx ", nic.packet[q]); 
    printf("\n");
  //}
}   

static unsigned short ipchksum(unsigned short *ip, int len)
{
  unsigned long sum = 0;
  len >>= 1;
  while (len--)
    {
      sum += *(ip++);
      if (sum > 0xFFFF)
        sum -= 0xFFFF;
    }
  return((~sum) & 0x0000FFFF);
}

#if 0
#define IPADDR(a,b,c,d) ((a)<<24)+((b)<<16)+((c)<<8)+(d)
#else
#define IPADDR(d,c,b,a) ((a)<<24)+((b)<<16)+((c)<<8)+(d)
#endif

struct node
  {
    char* name;
    arptable_t arp;
  };

#define NB_NODES 11
#define MCASTGROUP (NB_NODES-3)
#define BROADCAST (NB_NODES-2)
#define ZERO (NB_NODES-1)

struct node nodes[NB_NODES] =
{
#if 0

  {
    "firewall",
    {
      { IPADDR(192,168,0,1) },
      { 0x00, 0x80, 0xC8, 0xF3, 0x82, 0xB3 }
    }
  },
  {
    "compaq",
    {
      { IPADDR(192,168,0,2) },
      { 0x00, 0x08, 0xC7, 0xA2, 0xB9, 0x65 }
    }
  },
  {
    "k7",
    {
      { IPADDR(192,168,0,3) },
      { 0x00, 0xE0, 0x29, 0x54, 0x09, 0xD3 }
    }
  },
  {
    "mega",
    {
      { IPADDR(192,168,0,4) },
      { 0x00, 0x03, 0x93, 0x01, 0xBA, 0x52 }
    }
  },
  {
    "linksys_switch",
    {
      { IPADDR(192,168,1,1) },
      { 0x00, 0x04, 0x5A, 0xD2, 0x9C, 0xEB }
    }
  },
  {
    "mcastgroup", // check http://www.iana.org/assignments/multicast-addresses
    {
      { IPADDR(224,0,0,100) },
      { 0x01, 0x00, 0x5E, 0, 0, 100 }
    }
  },
  {
    "broadcast",
    {
      { IPADDR(192,168,1,255) },
      { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF }
    }
  },
  {
    "zero",
    {
      { IPADDR(0,0,0,0) },
      { 0, 0, 0, 0, 0, 0 }
    }
  }

#else

  {
    "dino00",
    {
      { IPADDR(132,204,25,170) },
      { 0x00, 0xD0, 0xB7, 0x68, 0xDC, 0x16 }
    }
  },
  {
    "dino01",
    {
      { IPADDR(132,204,25,171) },
      { 0x00, 0x90, 0x27, 0x44, 0x13, 0x49 }
    }
  },
  {
    "dino02",
    {
      { IPADDR(132,204,25,172) },
      { 0x00, 0xD0, 0xB7, 0x68, 0xE1, 0x8B }
    }
  },
  {
    "dino03",
    {
      { IPADDR(132,204,25,173) },
      { 0x00, 0x90, 0x27, 0x5A, 0xE4, 0xAC }
    }
  },
  {
    "dino04",
    {
      { IPADDR(132,204,25,174) },
      { 0x00, 0x90, 0x27, 0x5A, 0xE4, 0xAF }
    }
  },
  {
    "allo",
    {
      { IPADDR(132,204,26,156) },
      { 0x00, 0x01, 0x02, 0x36, 0xEB, 0xE8 }
    }
  },
  {
    "velo",
    {
      { IPADDR(132,204,26,158) },
      { 0x00, 0x50, 0xDA, 0xB4, 0x0C, 0xC2 }
    }
  },
  {
    "diplo",
    {
      { IPADDR(132,204,26,127) },
      { 0x00, 0x20, 0xAF, 0xF1, 0x38, 0x2C }
    }
  },
  {
    "mcastgroup", // check http://www.iana.org/assignments/multicast-addresses
    {
      { IPADDR(224,0,0,100) },
      { 0x01, 0x00, 0x5E, 0, 0, 100 }
    }
  },
  {
    "broadcast",
    {
      { IPADDR(132,204,27,255) },
      { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF }
    }
  },
  {
    "zero",
    {
      { IPADDR(0,0,0,0) },
      { 0, 0, 0, 0, 0, 0 }
    }
  }

#endif
};

bool equal_node_addr (uint8 node1[ETH_ALEN], uint8 node2[ETH_ALEN])
{
  for (int i=0; i<ETH_ALEN; i++)
    if (node1[i] != node2[i])
      return 0;
  return 1;
}

int find_node_id_from_node_addr (uint8 addr[ETH_ALEN])
{
  for (int node_id=0; node_id<NB_NODES; node_id++)
    if (equal_node_addr (addr, nodes[node_id].arp.node))
      return node_id;
  return -1;
}

int find_node_id_from_in_addr (in_addr addr)
{
  for (int node_id=0; node_id<NB_NODES; node_id++)
    if (addr.s_addr == nodes[node_id].arp.ipaddr.s_addr)
      return node_id;
  return -1;
}

struct ethhdr
  {
    uint8 dst_addr[ETH_ALEN];
    uint8 src_addr[ETH_ALEN];
    uint16 type;
  };

struct udp_packet_type1
  {
    struct iphdr iphdr;
    struct udphdr udphdr;
    int a;
    int b;
  };

struct udp_packet_type2
  {
    struct iphdr iphdr;
    struct udphdr udphdr;
    int c;
    int d;
  };

#define IP_ICMP 1
#define ICMP_ECHOREPLY 0
#define ICMP_ECHO      8

struct icmp_packet
  {
    struct iphdr iphdr;
    uint8 icmp_type;
    uint8 icmp_code;
    uint16 icmp_cksum;
    uint16 icmp_id;
    uint16 icmp_seq;
  };

#define IP_IGMP 2
#define IGMP_HOST_MEMBERSHIP_QUERY  0x11
#define IGMP_HOST_MEMBERSHIP_REPORT 0x12

struct igmp_packet
  {
    struct iphdr iphdr;
    uint8 igmp_type;
    uint8 igmp_code;
    uint16 igmp_cksum;
    in_addr igmp_group;
  };

#define ARPHDR_ETHER 1
#define ETHERTYPE_IP 0x0800

void arp_transmit
  (uint8 shwaddr[ETH_ALEN],
   in_addr sipaddr,
   uint8 thwaddr[ETH_ALEN],
   in_addr tipaddr,
   bool request)
{
  struct arprequest arp;

  arp.hwtype = htons (ARPHDR_ETHER);
  arp.protocol = htons (ETHERTYPE_IP);
  arp.hwlen = ETH_ALEN;
  arp.protolen = sizeof (in_addr);

  if (request)
    arp.opcode = htons (ARP_REQUEST);
  else
    arp.opcode = htons (ARP_REPLY);

  memcpy (arp.shwaddr, shwaddr, sizeof (arp.shwaddr));
  memcpy (arp.sipaddr, &sipaddr, sizeof (arp.sipaddr));
  memcpy (arp.thwaddr, thwaddr, sizeof (arp.thwaddr));
  memcpy (arp.tipaddr, &tipaddr, sizeof (arp.tipaddr));

  if (request)
    eth_transmit ((char*)nodes[BROADCAST].arp.node, ARP, sizeof (arp), &arp);
  else
    eth_transmit ((char*)thwaddr, ARP, sizeof (arp), &arp);
}

void icmp_transmit
  (void* buf,
   uint16 len,
   in_addr src,
   in_addr dst,
   uint8 dst_eth[ETH_ALEN],
   uint16 id,
   uint16 seq,
   bool echo)
{
  struct icmp_packet* icmp = (struct icmp_packet*)buf;

  if (echo)
    {
      icmp->iphdr.verhdrlen = 0x45;
      icmp->iphdr.service = 0;
      icmp->iphdr.len = htons (len);
      icmp->iphdr.ident = 0;
      icmp->iphdr.frags = 0;
      icmp->iphdr.ttl = 255;
      icmp->iphdr.protocol = IP_ICMP;
    }

  icmp->iphdr.src.s_addr = src.s_addr;
  icmp->iphdr.dest.s_addr = dst.s_addr;
  icmp->iphdr.chksum = 0;
  icmp->iphdr.chksum = ipchksum ((unsigned short *)icmp,
                                 sizeof (struct iphdr));

  if (echo)
    {
      icmp->icmp_type = ICMP_ECHO;
      icmp->icmp_code = 0;
      icmp->icmp_id = htons (id);
      icmp->icmp_seq = htons (seq);
    }
  else
    icmp->icmp_type = ICMP_ECHOREPLY;

  icmp->icmp_cksum = 0;
  icmp->icmp_cksum = ipchksum ((unsigned short *)&icmp->icmp_type,
                               len - sizeof (struct iphdr));

  eth_transmit ((const char*)dst_eth, IP, len, icmp);
}

void igmp_transmit
  (in_addr src,
   in_addr dst,
   uint8 dst_eth[ETH_ALEN])
{
  struct igmp_packet buf;
  struct igmp_packet* igmp = (struct igmp_packet*)&buf;
  uint16 len = sizeof (struct igmp_packet);

  igmp->iphdr.verhdrlen = 0x45;
  igmp->iphdr.service = 0;
  igmp->iphdr.len = htons (len);
  igmp->iphdr.ident = 0;
  igmp->iphdr.frags = 0;
  igmp->iphdr.ttl = 1;/////////////////////////////maybe needs to be higher
  igmp->iphdr.protocol = IP_IGMP;

  igmp->iphdr.src.s_addr = src.s_addr;
  igmp->iphdr.dest.s_addr = dst.s_addr;
  igmp->iphdr.chksum = 0;
  igmp->iphdr.chksum = ipchksum ((unsigned short *)igmp,
                                 sizeof (struct iphdr));

  igmp->igmp_type = IGMP_HOST_MEMBERSHIP_REPORT;
  igmp->igmp_code = 0;
  igmp->igmp_group.s_addr = dst.s_addr;

  igmp->igmp_cksum = 0;
  igmp->igmp_cksum = ipchksum ((unsigned short *)&igmp->igmp_type,
                               len - sizeof (struct iphdr));


  eth_transmit ((const char*)dst_eth, IP, len, igmp);
}

void igmp_transmit
  (int src_id,
   int dst_id,
   uint16 src_sock,
   uint16 dst_sock,
   int len,
   void* buf)
{
  struct igmp_packet igmp;

  igmp.iphdr.verhdrlen = 0x45;
  igmp.iphdr.service = 0;
  igmp.iphdr.len = htons(len);
  igmp.iphdr.ident = 0;
  igmp.iphdr.frags = 0;
  igmp.iphdr.ttl = 60;
  igmp.iphdr.protocol = IP_IGMP;
  igmp.iphdr.chksum = 0;
  igmp.iphdr.src.s_addr = nodes[src_id].arp.ipaddr.s_addr;
  igmp.iphdr.dest.s_addr = nodes[dst_id].arp.ipaddr.s_addr;
  igmp.iphdr.chksum = ipchksum((unsigned short *)buf, sizeof(struct iphdr));

  igmp.igmp_type = IGMP_HOST_MEMBERSHIP_REPORT;
  igmp.igmp_code = 0;
  igmp.igmp_cksum = 0;
  //  igmp.igmp_group = 0;//////////

  eth_transmit((const char*)nodes[dst_id].arp.node, IP, sizeof (igmp), &igmp);
}

void udp_transmit
  (int src_id,
   int dst_id,
   uint16 src_sock,
   uint16 dst_sock,
   int len,
   void* buf)
{
  struct iphdr *ip;
  struct udphdr *udp;

  ip = (struct iphdr *)buf;
  udp = (struct udphdr *)((char *)buf + sizeof(struct iphdr));
  ip->verhdrlen = 0x45;
  ip->service = 0;
  ip->len = htons(len);
  ip->ident = 0;
  ip->frags = 0;
  ip->ttl = 60;
  ip->protocol = IP_UDP;
  ip->chksum = 0;
  ip->src.s_addr = nodes[src_id].arp.ipaddr.s_addr;
  ip->dest.s_addr = nodes[dst_id].arp.ipaddr.s_addr;
  ip->chksum = ipchksum((unsigned short *)buf, sizeof(struct iphdr));
  udp->src = htons(src_sock);
  udp->dest = htons(dst_sock);
  udp->len = htons(len - sizeof(struct iphdr));
  udp->chksum = 0;
  eth_transmit((const char*)nodes[dst_id].arp.node, IP, len, buf);
}

//#define DEBUG 1

struct stats_struct
  {
    uint32 nb_polls;
    uint32 nb_ARP;
    uint32 nb_ICMP;
    uint32 nb_IGMP;
    uint32 nb_IP;
    uint32 nb_other;
  } stats;

void reset_stats ()
{
  stats.nb_polls = 0;
  stats.nb_ARP = 0;
  stats.nb_ICMP = 0;
  stats.nb_IGMP = 0;
  stats.nb_IP = 0;
  stats.nb_other = 0;
}

void print_stats (uint32 usecs)
{ 

  term_write(cout, usecs);
  term_write(cout, " usecs, polls=");
  term_write(cout, stats.nb_polls);
  
  term_write(cout, ", received: ARP=");
  term_write(cout, stats.nb_ARP);

  term_write(cout, " ICMP=");
  term_write(cout, stats.nb_ICMP);

  term_write(cout, " IGMP=");
  term_write(cout, stats.nb_IGMP);

  term_write(cout, " IP=");
  term_write(cout, stats.nb_IP);

  term_write(cout, " other=");
  term_write(cout, stats.nb_other);

  term_write(cout, "\n");
}

void exec_node (int node_id)
{
  bool send_igmp_report = TRUE;
  struct timeval now, last;

  gettimeofday (&last, NULL);

  term_c window = new_term(0, 420, 80, 3, &font::mono_6x9, L"window", true);

  term_write(cout, "This is ");
  term_write(cout, nodes[node_id].name );
  term_write(cout, "\n");

  reset_stats ();

  for (;;)
    {
      uint32 usecs;

      gettimeofday (&now, NULL);

      usecs = now.tv_usec - last.tv_usec
              + (now.tv_sec - last.tv_sec) * 1000000;

      if (usecs >= 5*(1000000))
        {
          print_stats (usecs);

          if (send_igmp_report)
            {
              igmp_transmit
                (nodes[node_id].arp.ipaddr,
                 nodes[MCASTGROUP].arp.ipaddr,
                 nodes[MCASTGROUP].arp.node);
              //send_igmp_report = FALSE;
            }

#if 0
          cout << "sending request\n";
          arp_transmit
            (nodes[node_id].arp.node,
             nodes[node_id].arp.ipaddr,
             nodes[ZERO].arp.node,
             nodes[2].arp.ipaddr,
             TRUE);
#endif

          reset_stats ();
          gettimeofday (&last, NULL);
        }

      stats.nb_polls++;

      if (eth_poll())
        {
          struct ethhdr* ep = (struct ethhdr*)nic.packet;
          uint16 type = ntohs (ep->type);

          switch (type)
            {
            case ARP:
              {
                stats.nb_ARP++;

                if (equal_node_addr (ep->dst_addr, nodes[BROADCAST].arp.node))
                  {
                    struct arprequest* p =
                      (struct arprequest*)(nic.packet+sizeof (struct ethhdr));

#if 0
                    cout << ntohs (p->hwtype) << ","
                         << ntohs (p->protocol) << ","
                         << p->hwlen << ","
                         << p->protolen << ","
                         << ntohs (p->opcode) << " ";
                    for (int i=0; i<6; i++) cout << p->shwaddr[i] << " ";
                    for (int i=0; i<4; i++) cout << p->sipaddr[i] << " ";
                    for (int i=0; i<6; i++) cout << p->thwaddr[i] << " ";
                    for (int i=0; i<4; i++) cout << p->tipaddr[i] << " ";
                    cout << "\n";
#endif

                    if (ntohs (p->hwtype) == ARPHDR_ETHER
                        && ntohs (p->protocol) == ETHERTYPE_IP
                        && p->hwlen == ETH_ALEN
                        && p->protolen == sizeof (in_addr)
                        && ntohs (p->opcode) == ARP_REQUEST
                        && ((in_addr*)p->tipaddr)->s_addr
                           == nodes[node_id].arp.ipaddr.s_addr)
                      {
                        arp_transmit
                          (nodes[node_id].arp.node,
                           *(in_addr*)p->tipaddr,
                           (uint8*)p->shwaddr,
                           *(in_addr*)p->sipaddr,
                           FALSE);
                      }
                  }
                break;
              }

            case IP:
              {
                struct iphdr* ip =
                  (struct iphdr*)(nic.packet+sizeof (struct ethhdr));

                //if (equal_node_addr (ep->dst_addr, nic.node_addr))

                uint8 protocol = ip->protocol;

                int src_node_id = find_node_id_from_in_addr (ip->src);
                int dest_node_id = find_node_id_from_in_addr (ip->dest);

                switch (protocol)
                  {
                  case IP_ICMP:
                    {
                      struct icmp_packet* p =
                        (struct icmp_packet*)(nic.packet+sizeof (struct ethhdr));

                      stats.nb_ICMP++;

#if 0
                      show_packet ();

                      cout << "ICMP: nic.packetlen=" << nic.packetlen << " sizeof(struct icmp_packet)=" << sizeof (struct icmp_packet) << "\n";
#endif

#if 0
                      cout << "icmp_type=" << p->icmp_type << " "
                           << "icmp_code=" << p->icmp_code << " "
                           << "icmp_cksum=" << ntohs (p->icmp_cksum) << " "
                           << "icmp_id=" << ntohs (p->icmp_id) << " "
                           << "icmp_seq=" << ntohs (p->icmp_seq) << "\n";
#endif

                      if (equal_node_addr (ep->dst_addr, nodes[MCASTGROUP].arp.node)
                          || equal_node_addr (ep->dst_addr, nodes[BROADCAST].arp.node)
                          || equal_node_addr (ep->dst_addr, nodes[node_id].arp.node))
                        {
                          if (p->icmp_type == ICMP_ECHO)
                            {
#if 0
                              cout << "sending ICMP echo reply\n";
#endif

                              icmp_transmit
                                (ip,
                                 ntohs (ip->len),
                                 nodes[node_id].arp.ipaddr,
                                 nodes[src_node_id].arp.ipaddr,
                                 nodes[src_node_id].arp.node,
                                 0,
                                 0,
                                 FALSE);
                            }
                        }

                      break;
                    }

                  case IP_IGMP:
                    {
                      struct igmp_packet* p =
                        (struct igmp_packet*)(nic.packet+sizeof (struct ethhdr));

                      stats.nb_IGMP++;

#if 0
                      show_packet ();

                      cout << "IGMP: nic.packetlen=" << nic.packetlen << " sizeof(struct igmp_packet)=" << sizeof (struct igmp_packet) << "\n";

                      int src_node_id = find_node_id_from_in_addr (ip->src);
                      int dest_node_id = find_node_id_from_in_addr (ip->dest);
                      cout << "src_node_id=" << src_node_id << "\n";
                      cout << "dest_node_id=" << dest_node_id << "\n";

                      cout << "igmp_type=" << p->igmp_type << " "
                           << "igmp_code=" << p->igmp_code << " "
                           << "igmp_cksum=" << ntohs (p->igmp_cksum) << " "
                           << "igmp_group=" << (int)ntohl (p->igmp_group.s_addr) << "\n";
#endif

                      send_igmp_report = TRUE;

                      break;
                    }

                  default:
                    {
                      stats.nb_IP++;

#if 0
                      show_packet ();

                      cout << "IP: protocol=" << protocol << " src_node_id=" << src_node_id << " dest_node_id=" << dest_node_id << "\n";
#endif
                    }
                  }
                break;
              }

            default:
              {
                stats.nb_other++;

#if 0
                show_packet ();

                cout << "other type: nic.packetlen=" << nic.packetlen << "\n";
#endif
              }
            }
        }
    }
}

/**********************************************************************/

#include "general.h"
#include "rtlib.h"
#include "term.h"

struct arptable_t arptable[MAX_ARP];

struct rom_info rom = { 0, 0};

extern "C" int console_getc ()
{
  return 0;
}

extern "C" void console_putc (int c)
{
  native_char str[2];
  str[0] = c;
  str[1] = '\0';
  term_write(cout, str);
}

extern "C" int console_ischar ()
{
  return 0;
}

/**********************************************************************/

void setup_net ()
{
  if (!eth_probe ())
    {
      term_write(cout, "No adaptoer found\n");
      return;
    }

  term_write(cout, "Adapter found\n");

  int node_id = find_node_id_from_node_addr (nic.node_addr);

  if (node_id >= 0)
    {
      exec_node (node_id);
      return;
    }
    
  term_write(cout, "Unknown node!\n");
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
