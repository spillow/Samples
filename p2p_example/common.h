#ifndef _COMMON_H_
#define _COMMON_H_

#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>

#include <map>
#include <vector>
#include <string>
using namespace std;

#define SOCKET_MAKE_ERROR -3
#define SOCKET_BIND_ERROR -4

#define ArraySize(x) (sizeof(x) / sizeof(x[0]))

#define MAX_MSG_LENGTH 2048

static const std::string join_cmd = "join";
static const std::string type_str = "type";

static const std::string EstablishNeighbor_cmd = "EstablishNeighbor";
static const std::string Query_cmd    = "Query";
static const std::string QueryHit_cmd = "QueryHit";

static const std::string help_cmd = "help";
static const std::string ls_cmd   = "ls";
static const std::string neighbors_cmd = "neighbors";
static const std::string query_cmd = "query";
static const std::string progress_cmd = "progress";
static const std::string review_cmd = "review";
static const std::string search_time_cmd = "search-time";

static const std::string object_name_str = "object_name";

static const std::string id_str = "id";
static const std::string ip_address_str = "ip_address";
static const std::string port_number_str = "port_number";
static const std::string neighbors_str = "neighbors";
static const std::string files_on_disk_str = "files_on_disk";

typedef struct NeighborData
{
  int id;
  std::string ip_address;
  int port_number;
} NeighborData;

typedef struct SelfData
{
  std::vector<NeighborData> neighbors;
  std::vector<std::string> files_on_disk;
  int id;
  std::string ip_address;
  int port_number;

  //not used in serialization
  int sequence_number; //incremented upon each query
} SelfData;

struct MessageID
{
  int peer_id;
  int sequence_number;
};

struct Query
{
  MessageID message_id;
  int ttl_value;
  std::string filename;
};

struct QueryHit
{
  MessageID message_id;
  std::string filename;
  NeighborData info_about_peer_with_file;
};

bool serialize_selfdata(std::string&, SelfData self_data, std::string object_name = "SelfData");
bool deserialize_selfdata(std::string, SelfData& self_data);

bool serialize_query(std::string& input, Query query);
bool deserialize_query(std::string input, Query& query);

bool serialize_queryhit(std::string& input, QueryHit query_hit);
bool deserialize_queryhit(std::string input, QueryHit& query_hit);

int initialize_socket(int port_number, struct sockaddr_in* addr);
bool send_message(int sockfd, std::string host, int port_number, std::string message);

#endif //_COMMON_H_
