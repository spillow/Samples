#include "common.h"
#include "peer.h"

#include "json/json.h"

#include <stdio.h>
#include <string.h>
#include <netdb.h>

#include <string>
#include <map>
using namespace std;

/////////////////////////////////////////////////////
//
// send_message()
//
// Sends a message (in this case, a JSON string) to
// the specified host at the given port over the given
// socket.
//
// Returns true on success.
//
bool send_message(int sockfd, std::string host, int port_number, std::string message)
{
  struct hostent *host_info;

  if ((host_info = gethostbyname(host.c_str())) == NULL) {
    return false;
  }

  struct sockaddr_in to_addr;

  bzero((char*)&to_addr, sizeof(to_addr));

	to_addr.sin_family = AF_INET;
	bcopy((char*)host_info->h_addr, (char*)&to_addr.sin_addr.s_addr, host_info->h_length);
	to_addr.sin_port = htons((unsigned short)port_number);
  memset(to_addr.sin_zero, '\0', sizeof to_addr.sin_zero);

  //SBP_FIXME: sendto() won't necessarily send the entire payload on the first try.  This
  //needs to be worked into a loop that will feed the entire message out.
  int bytes_sent = sendto(sockfd, message.c_str(), message.size()+1, 0,
                          (struct sockaddr*)&to_addr, sizeof(to_addr));

  //make note to see if this is an issue with the message sizes we send.
  if (bytes_sent != (int)(message.size()+1)) {
    printf("NOTE: the entire message was not sent in this pass\n");
  }

  if (bytes_sent < 0) {
    return false;
  }

  return true;
}

/////////////////////////////////////////////////////
//
// initialize_socket()
//
// initializes a socket for udp style communication
// by binding it to the given port.
//
// Returns the socket descriptor and fills in the
// sockaddr_in struct or the specified define on
// error.
//
int initialize_socket(int port_number, struct sockaddr_in* addr)
{
  int sockfd = socket(AF_INET, SOCK_DGRAM, 0);

  if (sockfd < 0) {
    return SOCKET_MAKE_ERROR;
  }

  addr->sin_family = AF_INET;
  addr->sin_port = htons((unsigned short)port_number);
  addr->sin_addr.s_addr = htonl(INADDR_ANY);
  memset(addr->sin_zero, '\0', sizeof addr->sin_zero);

  if (bind(sockfd, (const struct sockaddr*)addr, (socklen_t)sizeof(struct sockaddr)) < 0) {
    return SOCKET_BIND_ERROR;
  }

  return sockfd;
}

/////////////////////////////////////////////////////
//
// serialize_selfdata()
//
bool serialize_selfdata(std::string& output, SelfData self_data, std::string object_name)
{
  json_object* top_level_object = json_object_new_object();

  json_object_object_add(top_level_object, (char*)object_name_str.c_str(),
                         json_object_new_string((char*)object_name.c_str()));

  json_object* neighbor_array = json_object_new_array();

  for (std::vector<NeighborData>::iterator i=self_data.neighbors.begin();
       i != self_data.neighbors.end(); i++) {
    json_object* new_neighbor = json_object_new_object();
    json_object_object_add(new_neighbor, (char*)id_str.c_str(), json_object_new_int(i->id));
    json_object_object_add(new_neighbor, (char*)ip_address_str.c_str(),
                           json_object_new_string((char*)i->ip_address.c_str()));
    json_object_object_add(new_neighbor, (char*)port_number_str.c_str(), json_object_new_int(i->port_number));
    json_object_array_add(neighbor_array, new_neighbor);
  }

  json_object_object_add(top_level_object, (char*)neighbors_str.c_str(), neighbor_array);

  json_object* files_on_disk_array = json_object_new_array();
  for (std::vector<string>::iterator i=self_data.files_on_disk.begin();
       i != self_data.files_on_disk.end(); i++) {
    json_object_array_add(files_on_disk_array, json_object_new_string((char*)i->c_str()));
  }

  json_object_object_add(top_level_object, (char*)files_on_disk_str.c_str(), files_on_disk_array);
  json_object_object_add(top_level_object, (char*)id_str.c_str(), json_object_new_int(self_data.id));

  json_object_object_add(top_level_object, (char*)ip_address_str.c_str(),
                         json_object_new_string((char*)self_data.ip_address.c_str()));

  json_object_object_add(top_level_object, (char*)port_number_str.c_str(),
                         json_object_new_int(self_data.port_number));

  output = std::string(json_object_get_string(top_level_object));

  json_object_put(top_level_object);

  return true;
}

/////////////////////////////////////////////////////
//
// deserialize_selfdata()
//
bool deserialize_selfdata(std::string input, SelfData& self_data)
{
  json_object* msg_struct = json_tokener_parse((char*)input.c_str());

  if (is_error(msg_struct)) {
    return false;
  }

  json_object* neighbors = json_object_object_get(msg_struct, (char*)neighbors_str.c_str());

  int neighbors_len = json_object_array_length(neighbors);

  for (int i=0; i < neighbors_len; i++) {
    NeighborData current_neighbor;
    json_object* single_neighbor = json_object_array_get_idx(neighbors, i);

    current_neighbor.id = json_object_get_int(
                            json_object_object_get(single_neighbor, (char*)id_str.c_str()));
    current_neighbor.ip_address = std::string(json_object_get_string(
        json_object_object_get(single_neighbor, (char*)ip_address_str.c_str())));
    current_neighbor.port_number = json_object_get_int(
                                    json_object_object_get(single_neighbor, (char*)port_number_str.c_str()));
    self_data.neighbors.push_back(current_neighbor);
  }

  json_object* files_on_disk = json_object_object_get(msg_struct, (char*)files_on_disk_str.c_str());

  for (int i=0; i < json_object_array_length(files_on_disk); i++) {
    self_data.files_on_disk.push_back(
      std::string(json_object_get_string(json_object_array_get_idx(files_on_disk, i)))
      );
  }

  self_data.id = json_object_get_int(json_object_object_get(msg_struct, (char*)id_str.c_str()));

  self_data.ip_address  = json_object_get_string(
                            json_object_object_get(msg_struct, (char*)ip_address_str.c_str()));
  self_data.port_number = json_object_get_int(
                            json_object_object_get(msg_struct, (char*)port_number_str.c_str()));

  json_object_put(msg_struct);

  return true;
}

/////////////////////////////////////////////////////
//
// serialize_query()
//
bool serialize_query(std::string& input, Query query)
{
  json_object* top_level_object = json_object_new_object();

  json_object_object_add(top_level_object, (char*)object_name_str.c_str(), json_object_new_string("Query"));

  json_object* message_id_object = json_object_new_object();
  json_object_object_add(message_id_object, "peer_id",
                          json_object_new_int(query.message_id.peer_id));
  json_object_object_add(message_id_object, "sequence_number",
                          json_object_new_int(query.message_id.sequence_number));

  json_object_object_add(top_level_object, "message_id", message_id_object);
  json_object_object_add(top_level_object, "ttl_value", json_object_new_int(query.ttl_value));
  json_object_object_add(top_level_object, "filename",
                          json_object_new_string((char*)query.filename.c_str()));

  input = json_object_get_string(top_level_object);

  json_object_put(top_level_object);

  return true;
}

/////////////////////////////////////////////////////
//
// deserialize_query()
//
bool deserialize_query(std::string input, Query& query)
{
  json_object* msg_struct = json_tokener_parse((char*)input.c_str());

  if (is_error(msg_struct)) {
    return false;
  }

  json_object* message_id_object = json_object_object_get(msg_struct, "message_id");

  MessageID message_id;
  message_id.peer_id = json_object_get_int(json_object_object_get(message_id_object, "peer_id"));
  message_id.sequence_number = json_object_get_int(
                                json_object_object_get(message_id_object, "sequence_number"));

  query.message_id = message_id;
  query.ttl_value  = json_object_get_int(json_object_object_get(msg_struct, "ttl_value"));
  query.filename   = json_object_get_string(json_object_object_get(msg_struct, "filename"));

  json_object_put(msg_struct);

  return true;
}

/////////////////////////////////////////////////////
//
// serialize_queryhit()
//
bool serialize_queryhit(std::string& input, QueryHit query_hit)
{
  json_object* top_level_object = json_object_new_object();

  json_object_object_add(top_level_object, (char*)object_name_str.c_str(),
                           json_object_new_string("QueryHit"));

  json_object* message_id_object = json_object_new_object();
  json_object_object_add(message_id_object, "peer_id",
                          json_object_new_int(query_hit.message_id.peer_id));
  json_object_object_add(message_id_object, "sequence_number",
                          json_object_new_int(query_hit.message_id.sequence_number));

  json_object_object_add(top_level_object, "message_id", message_id_object);

  json_object* new_neighbor = json_object_new_object();

  json_object_object_add(new_neighbor, (char*)id_str.c_str(),
                         json_object_new_int(query_hit.info_about_peer_with_file.id));

  json_object_object_add(new_neighbor, (char*)ip_address_str.c_str(),
      json_object_new_string((char*)query_hit.info_about_peer_with_file.ip_address.c_str()));

  json_object_object_add(new_neighbor, (char*)port_number_str.c_str(),
                         json_object_new_int(query_hit.info_about_peer_with_file.port_number));

  json_object_object_add(top_level_object, "info_about_peer_with_file", new_neighbor);

  json_object_object_add(top_level_object, "filename",
    json_object_new_string((char*)query_hit.filename.c_str()));

  input = json_object_get_string(top_level_object);

  json_object_put(top_level_object);

  return true;
}

/////////////////////////////////////////////////////
//
// deserialize_queryhit()
//
bool deserialize_queryhit(std::string input, QueryHit& query_hit)
{
  json_object* msg_struct = json_tokener_parse((char*)input.c_str());

  if (is_error(msg_struct)) {
    return false;
  }

  json_object* message_id_object = json_object_object_get(msg_struct, "message_id");
  query_hit.message_id.peer_id = json_object_get_int(json_object_object_get(message_id_object, "peer_id"));
  query_hit.message_id.sequence_number =
    json_object_get_int(json_object_object_get(message_id_object, "sequence_number"));

  json_object* neighbor_data_object = json_object_object_get(msg_struct, "info_about_peer_with_file");
  query_hit.info_about_peer_with_file.id =
     json_object_get_int(json_object_object_get(neighbor_data_object, "id"));
  query_hit.info_about_peer_with_file.ip_address =
     json_object_get_string(json_object_object_get(neighbor_data_object, "ip_address"));
  query_hit.info_about_peer_with_file.port_number =
     json_object_get_int(json_object_object_get(neighbor_data_object, "port_number"));

  query_hit.filename = json_object_get_string(json_object_object_get(msg_struct, "filename"));

  json_object_put(msg_struct);

  return true;
}

