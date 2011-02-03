#include "common.h"

#include "json/json.h"

#include <stdio.h>
#include <stdlib.h>

#include <vector>
#include <string>
#include <iostream>
#include <map>
using namespace std;

#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>

#define FILE_NOT_FOUND -1
#define PARSE_ERROR    -2

struct Peer
{
  //these are extracted from the config file at startup
  std::vector<int> neighbors;
  std::vector<std::string> files_on_disk;
  int id;

  //these are unknown until the peer actually contacts the tracker
  //and will be filled in then.
  std::string ip_address;
  int port_number;
};

struct ConfigInfo
{
  //read from top to bottom in the config file, the list of
  //peer ids that are assigned as they come in.
  std::vector<int> join_order;

  //map from an id number to the information of that peer of that id
  //as read from the config file.
  std::map<int, struct Peer> peer_info;
};

/////////////////////////////////////////////////////
//
// respond_to_peer()
//
void respond_to_peer(int sockfd, struct sockaddr_in* from,
                     struct ConfigInfo* config_info, int join_order_index)
{
  //extract the ip address and port number from the joining peer and store
  //it in config_info so that future join members who will be neighbors with
  //this peer will know how to get in touch.

  std::string ip_addr = inet_ntoa(from->sin_addr);
  int port_number = ntohs(from->sin_port);

  int peer_id_to_assign = config_info->join_order[join_order_index];

  std::map<int, struct Peer>::iterator elem = config_info->peer_info.find(peer_id_to_assign);
  if (elem != config_info->peer_info.end()) {
    elem->second.ip_address  = ip_addr;
    elem->second.port_number = port_number;
  }
  else {
    printf("id: %d not found in configuration\n", peer_id_to_assign);
  }

  printf("got call from ip: %s, port: %d\n", (char*)ip_addr.c_str(), port_number);

  //now build up the SelfData struct, serialize it and fire it back out to the peer.
  SelfData self_data;
  self_data.files_on_disk = config_info->peer_info[peer_id_to_assign].files_on_disk;
  self_data.id = config_info->peer_info[peer_id_to_assign].id;
  for (std::vector<int>::iterator i=config_info->peer_info[peer_id_to_assign].neighbors.begin();
       i != config_info->peer_info[peer_id_to_assign].neighbors.end(); i++) {
    NeighborData curr_neighbor;
    Peer curr_peer = config_info->peer_info[*i];

    curr_neighbor.id = curr_peer.id;
    curr_neighbor.ip_address  = curr_peer.ip_address;
    curr_neighbor.port_number = curr_peer.port_number;

    self_data.neighbors.push_back(curr_neighbor);
  }

  self_data.ip_address  = ip_addr;
  self_data.port_number = port_number;

  std::string serialized_SelfData;
  if (serialize_selfdata(serialized_SelfData, self_data) == false) {
    printf("failed to serialize SelfData...\n");
    return;
  }

  send_message(sockfd, ip_addr, port_number, serialized_SelfData);
}

/////////////////////////////////////////////////////
//
// run_tracker()
//
// Initializes a socket bound to the given port and infinitely
// loops waiting for incoming calls from newly joining peers to
// send them the collection of neighbors they should join with.
//
static void run_tracker(int port_number, struct ConfigInfo* config_info)
{
  printf("starting tracker on port %d\n", port_number);

  struct sockaddr_in self_addr;

  int sockfd = initialize_socket(port_number, &self_addr);

  if (sockfd < 0) {
    printf("error initializing socket...\n");
    exit(1);
  }

  char msg_buffer[MAX_MSG_LENGTH];
  struct sockaddr_in from;
  socklen_t fromlen = sizeof(struct sockaddr_in);

  //this steps through the join order list as read from the config file until
  //all of the defined peers have joined.
  int join_order_index = 0;

  //loop waiting for incoming requests from newly joining clients
  fd_set rfds;
  while (1) {
    FD_ZERO(&rfds);
    FD_SET(sockfd, &rfds);

    printf("waiting for incoming connections...\n");
    if (select(sockfd+1, &rfds, NULL, NULL, NULL) < 0) {
      printf("error in select...\n");
      exit(1);
    }

    //if set then there is incoming data to be read from the socket.
    if (FD_ISSET(sockfd, &rfds)) {
      int bytes_recv = recvfrom(sockfd, msg_buffer, ArraySize(msg_buffer), 0,
                                (struct sockaddr*)&from, &fromlen);

      if (bytes_recv < 0) {
        printf("error in recvfrom()...\n");
      }

      //printf("received %d bytes\n", bytes_recv);
      //printf("messsage recv'd: %s\n", msg_buffer);

      //incoming messages with type == join_cmd will be serviced with the appropriate
      //set of neighbors.

      json_object* msg_struct = json_tokener_parse(msg_buffer);

      if (is_error(msg_struct)) {
        printf("invalid message received, not processing request\n");
        continue;
      }

      std::string msg_type = json_object_get_string(json_object_object_get(msg_struct, "type"));
      json_object_put(msg_struct);

      if (msg_type == join_cmd) {
        if (join_order_index >= (int)(config_info->join_order.size())) {
          printf("the maximum specified peers have joined; no more will be serviced...\n");
          continue;
        }

        //return the proper info for the joining peer.
        respond_to_peer(sockfd, &from, config_info, join_order_index);

        join_order_index++;
      }
    }
  }
}

/////////////////////////////////////////////////////
//
// parse_config_file()
//
// loads the config file and parses the information into the struct.
// Returns errors as defined above or 0 on success.
//
static int parse_config_file(char* config_path, struct ConfigInfo* config_info)
{
  FILE* pFile = fopen(config_path, "r");

  if (pFile == NULL) {
    return FILE_NOT_FOUND;
  }

  char line_buffer[80];

  //read the file in a line at a time
  for (;;) {
    char* ptr = fgets(line_buffer, ArraySize(line_buffer), pFile);

    if (feof(pFile) && (ptr == NULL)) {
      break;
    }

    //printf("%s", line_buffer);

    std::vector<std::string> sections;
    std::string line = line_buffer;

    boost::trim(line);

    //parse the line and add its information to the config struct.
    boost::split(sections, line, boost::is_any_of(":"));

    //ensure that peer id, neighbors, file listing fields are in place on the line.

    if (sections.size() != 3) {
      return PARSE_ERROR;
    }

    //trim the individual fields
    for (std::vector<string>::iterator str=sections.begin(); str != sections.end(); str++) {
      boost::trim(*str);
    }

    //parse the peer id

    int peer_id = -1;

    try {
        peer_id = boost::lexical_cast<int>(sections[0]);
    }
    catch(boost::bad_lexical_cast &) {
      return PARSE_ERROR;
    }

    //printf("peer id: %d\n", peer_id);

    //std::cout << "section 1: '" << sections[1] << "'" << std::endl;

    //parse the neighbors
    std::vector<int> neighbors;

    if (sections[1].size() != 0) {
      std::vector<std::string> neighbor_strings;

      boost::split(neighbor_strings, sections[1], boost::is_any_of("\t "));

      for (std::vector<string>::iterator neighbor_str=neighbor_strings.begin();
           neighbor_str != neighbor_strings.end(); neighbor_str++) {
        try {
            neighbors.push_back(boost::lexical_cast<int>(*neighbor_str));
        }
        catch(boost::bad_lexical_cast &) {
          return PARSE_ERROR;
        }
      }
    }

    //parse the file listing
    std::vector<std::string> files;

    if (sections[2].size() != 0) {
      std::vector<std::string> files_strings;

      boost::split(files_strings, sections[2], boost::is_any_of(";"));

      for (std::vector<string>::iterator file_str=files_strings.begin();
           file_str != files_strings.end(); file_str++) {
        boost::trim(*file_str);
        //std::cout << "'" << *file_str << "'" << std::endl;
        files.push_back(*file_str);
      }
    }

    struct Peer curr_peer;
    curr_peer.neighbors = neighbors;
    curr_peer.files_on_disk = files;
    curr_peer.id = peer_id;

    config_info->peer_info[peer_id] = curr_peer;
    config_info->join_order.push_back(peer_id);
  }

  fclose(pFile);

  return 0;
}

int main(int argc, char** argv)
{
  if (argc != 3) {
    printf("Usage: tracker <config file> <port number>\n");
    exit(1);
  }

  char* config_path = argv[1];
  char* port_string = argv[2];

  int port_number = atoi(port_string);

  if (port_number == 0) {
    printf("invalid port number\n");
    exit(1);
  }

  struct ConfigInfo config_info;

  int status = parse_config_file(config_path, &config_info);

  if (status == FILE_NOT_FOUND) {
    printf("couldn't find config file at: %s\n", config_path);
    exit(1);
  }
  else if (status == PARSE_ERROR) {
    printf("error parsing config file\n");
    exit(1);
  }

  run_tracker(port_number, &config_info);

  return 0;
}

