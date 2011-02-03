#include "common.h"
#include "peer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <iostream>
#include <algorithm>
using namespace std;

#include <boost/algorithm/string.hpp>

#include "json/json.h"

#include <vector>

#define TRACKER_CONTACT_TIMEOUT 5 //seconds

/////////////////////////////////////////////////////
//
// ConnectionInfo::ConnectionInfo()
//
ConnectionInfo::ConnectionInfo(std::string ip, int port_number) : ip(ip), port_number(port_number) {}

/////////////////////////////////////////////////////
//
// obtain_config_info_from_tracker()
//
bool obtain_config_info_from_tracker(int sockfd, char* tracker_ip,
                                     int tracker_port_number, SelfData& self_data)
{
  json_object* outgoing_msg = json_object_new_object();
  json_object_object_add(outgoing_msg, (char*)type_str.c_str(),
                         json_object_new_string((char*)join_cmd.c_str()));
  std::string join_msg = json_object_get_string(outgoing_msg);
  json_object_put(outgoing_msg);

  if (send_message(sockfd, tracker_ip, tracker_port_number, join_msg) == false) {
    return false;
  }

  //wait until the specified timeout for a response back from the tracker and ensure
  //that the message received is indeed from the tracker.

  //SBP_FIXME: check that the tracker is the responder.

  struct timeval tv;
  tv.tv_sec  = TRACKER_CONTACT_TIMEOUT;
  tv.tv_usec = 0;

  fd_set rfds;
  FD_ZERO(&rfds);
  FD_SET(sockfd, &rfds);

  int ret = select(sockfd+1, &rfds, NULL, NULL, &tv);

  if (ret < 0) {
    printf("select error...\n");
    exit(1);
  }
  else if (ret == 0) { //succeeded but no descriptors ready ==> select timed out
    printf("timed out waiting for response from tracker...\n");
    exit(1);
  }

  char msg_buffer[MAX_MSG_LENGTH];
  struct sockaddr from;
  socklen_t fromlen = sizeof(struct sockaddr);

  if (FD_ISSET(sockfd, &rfds)) {
    int bytes_recv = recvfrom(sockfd, msg_buffer, ArraySize(msg_buffer), 0, &from, &fromlen);

    if (bytes_recv < 0) {
      printf("error receiving from tracker...\n");
      exit(1);
    }

    //printf("received %d bytes\n", bytes_recv);
    //printf("messsage recv'd: %s\n", msg_buffer);

    //SBP_FIXME:
    //check that it is a SelfData struct coming in.

    //attempt to deserialize the incoming message.
    if (deserialize_selfdata(msg_buffer, self_data) == false) {
      printf("error deserializing join request...\n");
      return false;
    }

    //print out the information received to make sure it matches after deserialization.
    printf("***assignment received from tracker***\n");
    printf("node id: %d\n", self_data.id);
    printf("neighbors: [ ");
    for (int i=0; i < (int)self_data.neighbors.size(); i++) {
      printf("%d ", self_data.neighbors[i].id);
    }
    printf("]\n");
    printf("files contained: [ ");
    for (int i=0; i < (int)self_data.files_on_disk.size(); i++) {
      printf("%s ", (char*)self_data.files_on_disk[i].c_str());
    }
    printf("]\n");
    //printf("my ip: %s\n", (char*)self_data.ip_address.c_str());
    //printf("my port number: %d\n", self_data.port_number);
  }

  //now send out the neighbor requests to all of the newly discovered neighbors.
  std::string establish_neighbor_request_string;
  serialize_selfdata(establish_neighbor_request_string,
                     self_data,
                     EstablishNeighbor_cmd);

  for (std::vector<NeighborData>::iterator neighbor=self_data.neighbors.begin();
       neighbor != self_data.neighbors.end(); neighbor++) {
    std::cout << "sending ESTABLISH-NEIGHBOR to ip: " << neighbor->ip_address << ", port: "
              << neighbor->port_number << std::endl;
    send_message(sockfd, neighbor->ip_address, neighbor->port_number, establish_neighbor_request_string);
  }

  return true;
}

/////////////////////////////////////////////////////
//
// main()
//
int main(int argc, char** argv)
{
  if (argc != 4) {
    printf("Usage: peer <port number> <tracker ip|name> <tracker port number>\n");
    exit(1);
  }

  char* port_string = argv[1];
  char* tracker_ip = argv[2];
  char* tracker_port_string = argv[3];

  int port_number = atoi(port_string);

  if (port_number == 0) {
    printf("invalid port number\n");
    exit(1);
  }

  int tracker_port_number = atoi(tracker_port_string);

  if (tracker_port_number == 0) {
    printf("invalid tracker port number\n");
    exit(1);
  }

  struct sockaddr_in self_addr;

  int sockfd = initialize_socket(port_number, &self_addr);

  if (sockfd < 0) {
    printf("error hooking up socket...\n");
    exit(1);
  }

  SelfData self_data;
  self_data.sequence_number = 0;

  //contact the tracker to obtain neighbor info
  if (obtain_config_info_from_tracker(sockfd, tracker_ip, tracker_port_number, self_data) == false) {
    printf("error communicating with tracker...\n");
    exit(1);
  }

  std::map<std::string, StdinHandler> stdin_dispatcher;
  std::map<std::string, RecvHandler>  recv_dispatcher;

  stdin_dispatcher[help_cmd]        = help_handler;
  stdin_dispatcher[ls_cmd]          = listing_handler;
  stdin_dispatcher[neighbors_cmd]   = neighbors_handler;
  stdin_dispatcher[query_cmd]       = query_handler;
  stdin_dispatcher[progress_cmd]    = progress_handler;
  stdin_dispatcher[review_cmd]      = review_handler;
  stdin_dispatcher[search_time_cmd] = search_time_handler;

  recv_dispatcher[EstablishNeighbor_cmd] = establish_neighbor_handler;
  recv_dispatcher[Query_cmd]             = query_receive_handler;
  recv_dispatcher[QueryHit_cmd]          = queryhit_receive_handler;

  printf("running main event loop\n");
  main_event_loop(sockfd, stdin_dispatcher, recv_dispatcher, self_data);

  return 0;
}

/////////////////////////////////////////////////////
//
// search_time_handler()
//
bool search_time_handler(std::vector<string> arguments,
                         std::string& data_out,
                         std::vector<ConnectionInfo>& receivers,
                         int timer_time_remaining,
                         int& set_new_time_remaining,
                         CommunicationState& comm_state,
                         SelfData& self_data)
{
  if (arguments.size() > 1) {
    printf("search-time <new-time? (seconds)>\n");
    return false;
  }

  if (arguments.size() == 0) {
    printf("SEARCH-TIMEOUT set to: %d seconds\n", comm_state.search_timeout);
    return true;
  }

  int new_search_timeout = atoi((char*)arguments[0].c_str());

  if (new_search_timeout == 0) {
    printf("invalid time set for SEARCH-TIMEOUT\n");
    return false;
  }

  comm_state.search_timeout = new_search_timeout;
  printf("setting SEARCH-TIMEOUT to: %d seconds\n", comm_state.search_timeout);

  return true;
}

/////////////////////////////////////////////////////
//
// review_handler()
//
bool review_handler(std::vector<string> arguments,
                    std::string& data_out,
                    std::vector<ConnectionInfo>& receivers,
                    int timer_time_remaining,
                    int& set_new_time_remaining,
                    CommunicationState& comm_state,
                    SelfData& self_data)
{
  if (arguments.size() != 1) {
    printf("usage: review <filename>\n");
    return false;
  }

  std::string filename = arguments[0];

  printf("owners of file: %s\n", (char*)filename.c_str());

  std::map<std::string, std::vector<NeighborData> >::iterator iter =
    comm_state.who_has_what_file.find(filename);

  if (iter == comm_state.who_has_what_file.end()) {
    printf("none found to have this file\n");
  }
  else {
    for (std::vector<NeighborData>::iterator neighbor=iter->second.begin();
         neighbor != iter->second.end();
         neighbor++) {
      printf("[peer id: %d, ip: %s, port: %d]\n",
             neighbor->id,
             (char*)neighbor->ip_address.c_str(),
             neighbor->port_number);
    }
  }

  return true;
}

/////////////////////////////////////////////////////
//
// progress_handler()
//
bool progress_handler(std::vector<string> arguments,
                      std::string& data_out,
                      std::vector<ConnectionInfo>& receivers,
                      int timer_time_remaining,
                      int& set_new_time_remaining,
                      CommunicationState& comm_state,
                      SelfData& self_data)
{
  if (arguments.size() != 0) {
    printf("progress takes no arguments\n");
    return false;
  }

  if (comm_state.query_in_progress == false) {
    printf("no query is in progress\n");
    return true;
  }

  printf("owners of file: %s\n", (char*)comm_state.file_queried_for.c_str());

  std::map<std::string, std::vector<NeighborData> >::iterator iter =
    comm_state.who_has_what_file.find(comm_state.file_queried_for);

  if (iter == comm_state.who_has_what_file.end()) {
    printf("none yet\n");
  }
  else {
    for (std::vector<NeighborData>::iterator neighbor=iter->second.begin();
         neighbor != iter->second.end();
         neighbor++) {
      printf("[peer id: %d, ip: %s, port: %d]\n",
             neighbor->id,
             (char*)neighbor->ip_address.c_str(),
             neighbor->port_number);
    }
  }

  printf("search time remaining: %d seconds\n", timer_time_remaining);

  return true;
}

/////////////////////////////////////////////////////
//
// query_handler()
//
bool query_handler(std::vector<string> arguments,
                   std::string& data_out,
                   std::vector<ConnectionInfo>& receivers,
                   int timer_time_remaining,
                   int& set_new_time_remaining,
                   CommunicationState& comm_state,
                   SelfData& self_data)
{
  if ((arguments.size() == 1 || arguments.size() == 2) == false) {
    printf("usage: %s <filename> <ttl-value?>\n", (char*)query_cmd.c_str());
    return false;
  }

  std::string filename = arguments[0];

  //check to see if this file is already on disk.  If it is, abort the query.
  if (std::find(self_data.files_on_disk.begin(), self_data.files_on_disk.end(), filename) !=
      self_data.files_on_disk.end()) {
    printf("I already have it\n");
    return true;
  }

  //currenly disallow multiple queries simultaneously.  If one is in
  //progress, just return the time remaining for it.
  if (comm_state.query_in_progress == true) {
    if (timer_time_remaining < 0) {
      printf("query currently in progress, try again later\n");
    }
    else {
      printf("query currently in progress, remaining: %d seconds\n", timer_time_remaining);
    }
    return true;
  }

  if (arguments.size() == 2) {
    int new_ttl_value = atoi((char*)arguments[1].c_str());

    if (new_ttl_value == 0) {
      printf("invalid ttl value specified\n");
      return false;
    }

    comm_state.ttl_value = new_ttl_value;
  }

  //since we are starting a new file query, clear out the listing of who has this particular file
  //so that it can be repopulated again.
  std::map<string, std::vector<NeighborData> >::iterator elem =
    comm_state.who_has_what_file.find(filename);

  if (elem != comm_state.who_has_what_file.end()) {
    comm_state.who_has_what_file.erase(elem);
  }

  //initiate the query
  comm_state.query_in_progress = true;
  comm_state.file_queried_for  = filename;

  self_data.sequence_number++;
  set_new_time_remaining = comm_state.search_timeout;

  //Add this message to the record of queries so that, if the query comes back to
  //us again, we don't waste another step and send it out to our neighbors again
  //(even though they would receive it and immediately drop it on the floor, this
  //prevents wasting that one last hop).

  std::map<int, std::map<int, ConnectionInfo> >::iterator first_map =
    comm_state.past_received_queries_record.find(self_data.id);

  ConnectionInfo info(self_data.ip_address, self_data.port_number);

  if (first_map == comm_state.past_received_queries_record.end()) {
    std::map<int, ConnectionInfo> ci_map;

    ci_map[self_data.sequence_number] = info;
    comm_state.past_received_queries_record[self_data.id] = ci_map;
  }
  else {
    first_map->second[self_data.sequence_number] = info;
  }

  Query query;

  MessageID message_id;
  message_id.peer_id = self_data.id;
  message_id.sequence_number = self_data.sequence_number;
  query.ttl_value = comm_state.ttl_value;
  query.filename = filename;
  query.message_id = message_id;

  serialize_query(data_out, query);

  printf("sending QUERY to neighbors for %s with ttl = %d\n", (char*)filename.c_str(), comm_state.ttl_value);

  //load up all neighbors as the receivers of the query.
  for (std::vector<NeighborData>::iterator neighbor=self_data.neighbors.begin();
       neighbor != self_data.neighbors.end();
       neighbor++) {
    ConnectionInfo info(neighbor->ip_address, neighbor->port_number);
    receivers.push_back(info);
  }

  return true;
}

/////////////////////////////////////////////////////
//
// listing_handler()
//
bool listing_handler(std::vector<string> arguments,
                     std::string& data_out,
                     std::vector<ConnectionInfo>& receivers,
                     int timer_time_remaining,
                     int& set_new_time_remaining,
                     CommunicationState& comm_state,
                     SelfData& self_data)
{
  if (arguments.size() != 0) {
    printf("%s takes no arguments\n", (char*)ls_cmd.c_str());
    return false;
  }

  printf("files on disk:\n");

  for (std::vector<string>::iterator file=self_data.files_on_disk.begin();
       file != self_data.files_on_disk.end();
       file++) {
    printf("%s ", (char*)file->c_str());
  }

  printf("\n");

  return true;
}

/////////////////////////////////////////////////////
//
// neighbors_handler()
//
// prints out the peer id values of the current set
// of neighbors.
//
bool neighbors_handler(std::vector<string> arguments,
                       std::string& data_out,
                       std::vector<ConnectionInfo>& receivers,
                       int timer_time_remaining,
                       int& set_new_time_remaining,
                       CommunicationState& comm_state,
                       SelfData& self_data)
{
  if (arguments.size() != 0) {
    printf("%s takes no arguments\n", (char*)neighbors_cmd.c_str());
    return false;
  }

  printf("neighbors: [ ");
  for (std::vector<NeighborData>::iterator neighbor=self_data.neighbors.begin();
       neighbor != self_data.neighbors.end();
       neighbor++) {
    printf("%d ", neighbor->id);
  }
  printf("]\n");

  return true;
}

/////////////////////////////////////////////////////
//
// help_handler()
//
bool help_handler(std::vector<string> arguments,
                  std::string& data_out,
                  std::vector<ConnectionInfo>& receivers,
                  int timer_time_remaining,
                  int& set_new_time_remaining,
                  CommunicationState& comm_state,
                  SelfData& self_data)
{
  if (arguments.size() != 0) {
    printf("%s takes no arguments\n", (char*)help_cmd.c_str());
    return false;
  }

  printf("list of commands:\n");
  printf("ls\n");
  printf("neighbors\n");
  printf("query <filename> <ttl-value?>\n");
  printf("progress\n");
  printf("review <filename>\n");
  printf("search-time <new-time? (seconds)>\n");

  return true;
}

/////////////////////////////////////////////////////
//
// queryhit_receive_handler()
//
//
bool queryhit_receive_handler(ConnectionInfo caller,
                              std::string data_in,
                              std::string& data_out,
                              std::vector<ConnectionInfo>& receivers,
                              int timer_time_remaining,
                              int& set_new_time_remaining,
                              CommunicationState& comm_state,
                              SelfData& self_data)
{
  QueryHit query_hit;

  if (deserialize_queryhit(data_in, query_hit) == false) {
    printf("error parsing QUERYHIT...\n");
    return false;
  }

  //if we are the originator of the query, add to the listing of
  //what peers have this file.
  if (query_hit.message_id.peer_id == self_data.id) {
    std::map<std::string, std::vector<NeighborData> >::iterator elem =
      comm_state.who_has_what_file.find(query_hit.filename);

    if (elem == comm_state.who_has_what_file.end()) {
      std::vector<NeighborData> file_owners;
      file_owners.push_back(query_hit.info_about_peer_with_file);

      comm_state.who_has_what_file[query_hit.filename] = file_owners;
    }
    else {
      elem->second.push_back(query_hit.info_about_peer_with_file);
    }

    printf("QUERYHIT for %s returned to querier\n", (char*)query_hit.filename.c_str());

    //add the file name to our own local store if it is not already there.
    bool file_present = false;
    for (std::vector<string>::iterator f=self_data.files_on_disk.begin();
         f != self_data.files_on_disk.end();
         f++) {
      if (*f == query_hit.filename) {
        file_present = true;
        break;
      }
    }

    if (file_present == false) {
      self_data.files_on_disk.push_back(query_hit.filename);
    }
  }
  else {
    //we didn't issue the query; pass it back to the neighbor that
    //propagated it to us.

    std::map<int, std::map<int, ConnectionInfo> >::iterator first_map =
      comm_state.past_received_queries_record.find(query_hit.message_id.peer_id);

    if (first_map == comm_state.past_received_queries_record.end()) {
      printf("error: received QUERYHIT return though there is no record of this message\n");
      return false;
    }

    std::map<int, ConnectionInfo>::iterator second_map =
      first_map->second.find(query_hit.message_id.sequence_number);

    if (second_map == first_map->second.end()) {
      printf("error: received QUERYHIT return though there is no record of this message\n");
      return false;
    }

    receivers.push_back(second_map->second);
    data_out = data_in;
  }

  return true;
}

/////////////////////////////////////////////////////
//
// query_receive_handler()
//
bool query_receive_handler(ConnectionInfo caller,
                           std::string data_in,
                           std::string& data_out,
                           std::vector<ConnectionInfo>& receivers,
                           int timer_time_remaining,
                           int& set_new_time_remaining,
                           CommunicationState& comm_state,
                           SelfData& self_data)
{
  Query query;
  if (deserialize_query(data_in, query) == false) {
    printf("error parsing query...\n");
    return false;
  }

  bool propagate_query = true;

  //check to see if we've seen this message before.
  std::map<int, std::map<int, ConnectionInfo> >::iterator first_map =
    comm_state.past_received_queries_record.find(query.message_id.peer_id);

  if (first_map == comm_state.past_received_queries_record.end()) { //haven't seen this peer_id
    std::map<int, ConnectionInfo> ci_map;
    ci_map[query.message_id.sequence_number] = caller;
    comm_state.past_received_queries_record[query.message_id.peer_id] = ci_map;
  }
  else {
    std::map<int, ConnectionInfo>::iterator second_map = first_map->second.find(query.message_id.sequence_number);
    if (second_map == first_map->second.end()) {
      first_map->second[query.message_id.sequence_number] = caller;
    }
    else {
      //we've seen this message before, don't propagate it along again.
      propagate_query = false;
    }
  }

  if (propagate_query == true && query.ttl_value > 0) {
    query.ttl_value--;

    if (serialize_query(data_out, query) == false) {
      printf("error serializing query...\n");
    }
    else {
      printf("unseen QUERY: propagating request to neighbors\n");
      for (std::vector<NeighborData>::iterator neighbor=self_data.neighbors.begin();
          neighbor != self_data.neighbors.end();
          neighbor++) {
        ConnectionInfo info(neighbor->ip_address, neighbor->port_number);

        //don't send a query back to the neighbor that just sent it to us.
        if (caller.ip          == info.ip &&
            caller.port_number == info.port_number) {
        }
        else {
          receivers.push_back(info);
        }
      }
    }
  }

  //respond back with a QueryHit if we have the file.
  if (propagate_query == true) {
    bool file_found = false;
    for (std::vector<string>::iterator file=self_data.files_on_disk.begin();
         file != self_data.files_on_disk.end();
         file++) {
      if (query.filename == *file) {
        file_found = true;
        break;
      }
    }

    //SBP_FIXME: should we propagate the QUERYHIT back to everybody who calls or
    //just the first time that we see it?
    if (file_found == true) {
      std::string queryhit_out;
      QueryHit query_hit;
      query_hit.message_id = query.message_id;
      query_hit.info_about_peer_with_file.id = self_data.id;
      query_hit.info_about_peer_with_file.ip_address = self_data.ip_address;
      query_hit.info_about_peer_with_file.port_number = self_data.port_number;
      query_hit.filename = query.filename;

      serialize_queryhit(queryhit_out, query_hit);

      printf("returning QUERYHIT for file: %s\n", (char*)query.filename.c_str());
      send_message(comm_state.sockfd, caller.ip, caller.port_number, queryhit_out);
    }
  }

  return true;
}

/////////////////////////////////////////////////////
//
// establish_neighbor_handler()
//
bool establish_neighbor_handler(ConnectionInfo caller,
                                std::string data_in,
                                std::string& data_out,
                                std::vector<ConnectionInfo>& receivers,
                                int timer_time_remaining,
                                int& set_new_time_remaining,
                                CommunicationState& comm_state,
                                SelfData& self_data)
{
  SelfData incoming_neighbor_data;

  if (deserialize_selfdata(data_in, incoming_neighbor_data) == false) {
    return false;
  }

  printf("adding neighbor with peer id: %d to the neighbor list\n", incoming_neighbor_data.id);

  NeighborData new_neighbor;
  new_neighbor.id = incoming_neighbor_data.id;
  new_neighbor.ip_address  = incoming_neighbor_data.ip_address;
  new_neighbor.port_number = incoming_neighbor_data.port_number;

  self_data.neighbors.push_back(new_neighbor);

  return true;
}

/////////////////////////////////////////////////////
//
// in_set_of_peers()
//
// Returns true if the the node with the given ip
// and port number is in self_data's set of neighbors.
// Updates peer_id with the id of the neighbor if found.
//
bool in_set_of_neighbors(std::string ip_address, int port_number,
                         int& peer_id, SelfData& self_data)
{
  for (std::vector<NeighborData>::iterator neighbor=self_data.neighbors.begin();
       neighbor != self_data.neighbors.end();
       neighbor++) {
    if (neighbor->ip_address  == ip_address &&
        neighbor->port_number == port_number) {
      peer_id = neighbor->id;
      return true;
    }
  }

  return false;
}

/////////////////////////////////////////////////////
//
// parse_command_input()
//
void parse_command_input(char* input_string, std::string& command, std::vector<string>& arguments)
{
  std::vector<string> sections;

  boost::split(sections, input_string, boost::is_any_of(" "));

  if (sections.size() == 0) {
    return;
  }

  boost::trim(sections[0]);
  command = sections[0];

  for (int i=1; i < (int)sections.size(); i++) {
    boost::trim(sections[i]);

    if (sections[i] != "") {
      arguments.push_back(sections[i]);
    }
  }
}

/////////////////////////////////////////////////////
//
// main_event_loop()
//
void main_event_loop(int sockfd,
                     std::map<std::string, StdinHandler> stdin_dispatcher,
                     std::map<std::string, RecvHandler> recv_dispatcher,
                     SelfData& self_data)
{
  char msg_buffer_stdin[MAX_MSG_LENGTH];

  printf(">");
  fflush(stdout);

  CommunicationState comm_state;
  comm_state.sockfd = sockfd;
  comm_state.query_in_progress = false;
  comm_state.search_timeout = 30; //seconds
  comm_state.ttl_value = 20;

  struct timeval tv;
  tv.tv_sec  = 10;
  tv.tv_usec =  0;

  struct timeval* current_timer = NULL;

  fd_set rfds;
  while (1) {
    FD_ZERO(&rfds);
    FD_SET(sockfd, &rfds);
    FD_SET(0, &rfds); //stdin

    int select_status = select(sockfd+1, &rfds, NULL, NULL, current_timer);

    if (select_status < 0) {
      printf("error in select...\n");
      exit(1);
    }
    else if (select_status == 0) { //timeout
      printf("query search complete...\n");
      comm_state.query_in_progress = false;
      current_timer = NULL;
      fflush(stdout);
    }

    //handle stdin action
    if (FD_ISSET(0, &rfds)) {
      int bytes_read = read(0, msg_buffer_stdin, ArraySize(msg_buffer_stdin)-1);
      if (bytes_read <= 0) {
        printf("read no bytes\n");
      }
      else {
        msg_buffer_stdin[bytes_read] = '\0';
        if (msg_buffer_stdin[0] == '\n' || msg_buffer_stdin[0] == '\r') {
          printf(">");
          fflush(stdout);
        }
        else {
          //wipe the new line for string matching
          msg_buffer_stdin[bytes_read-1] = '\0';
          //printf("got string: '%s'\n", msg_buffer);

          //break the input string into the name of the command and its arguments.
          std::string command;
          std::vector<string> arguments;

          parse_command_input(msg_buffer_stdin, command, arguments);

          //dispatch based on the string

          std::map<std::string, StdinHandler>::iterator elem = stdin_dispatcher.find(command);
          if (elem != stdin_dispatcher.end()) {
            std::string data_out;
            std::vector<ConnectionInfo> receivers;

            int curr_time_remaining = (current_timer == NULL) ? -1 : tv.tv_sec;
            int new_time_remaining  = curr_time_remaining;

            bool status = elem->second(arguments, data_out, receivers,
                                       curr_time_remaining, new_time_remaining, comm_state, self_data);

            if (new_time_remaining != curr_time_remaining) {
              if (new_time_remaining < 0) {
                current_timer = NULL;
              }
              else {
                current_timer = &tv;
                tv.tv_sec  = new_time_remaining;
                tv.tv_usec = 0;
              }
            }

            if (status == false) {
              printf("error handling %s\n", (char*)command.c_str());
            }
            else {
              //now respond with the message to the appropriate receivers as determined by the handler.
              for (std::vector<ConnectionInfo>::iterator receiver=receivers.begin();
                  receiver != receivers.end();
                  receiver++) {
                send_message(sockfd, receiver->ip, receiver->port_number, data_out);
              }
            }
          }
          else {
            printf("command not found -- type 'help'\n");
          }

          printf(">");
          fflush(stdout);
        }
      }
    }

    char msg_buffer_recv[MAX_MSG_LENGTH];

    struct sockaddr_in from;
    socklen_t fromlen = sizeof(struct sockaddr_in);

    //handle network action
    if (FD_ISSET(sockfd, &rfds)) {
      int bytes_recv = recvfrom(sockfd, msg_buffer_recv, ArraySize(msg_buffer_recv), 0,
                                (struct sockaddr*)&from, &fromlen);

      if (bytes_recv < 0) {
        printf("error in recvfrom()...\n");
        continue;
      }

      json_object* msg_struct = json_tokener_parse(msg_buffer_recv);

      if (is_error(msg_struct)) {
        printf("invalid message received, not processing request\n");
        continue;
      }

      json_object* obj_name = json_object_object_get(msg_struct, (char*)object_name_str.c_str());
      if (obj_name == NULL) {
        printf("invalid message format\n");
        json_object_put(msg_struct);
        continue;
      }

      std::string dispatch_name = json_object_get_string(obj_name);
      json_object_put(msg_struct);

      printf("%s message received from ip: %s, port: %d\n",
             (char*)dispatch_name.c_str(),
             inet_ntoa(from.sin_addr),
             ntohs(from.sin_port));

      std::map<std::string, RecvHandler>::iterator elem = recv_dispatcher.find(dispatch_name);
      if (elem == recv_dispatcher.end()) {
        printf("cannot handle incoming message of type: %s\n", (char*)dispatch_name.c_str());
      }
      else {
        ConnectionInfo caller(inet_ntoa(from.sin_addr), ntohs(from.sin_port));
        std::string data_out;
        std::vector<ConnectionInfo> receivers;

        int curr_time_remaining = (current_timer == NULL) ? -1 : tv.tv_sec;
        int new_time_remaining  = curr_time_remaining;

        bool status = elem->second(caller, msg_buffer_recv, data_out,
                                   receivers, curr_time_remaining, new_time_remaining,
                                   comm_state, self_data);

        if (new_time_remaining != curr_time_remaining) {
          if (new_time_remaining < 0) {
            current_timer = NULL;
          }
          else {
            current_timer = &tv;
            tv.tv_sec  = new_time_remaining;
            tv.tv_usec = 0;
          }
        }

        if (status == false) {
          printf("error handling %s\n", (char*)dispatch_name.c_str());
        }
        else {
          //now respond with the message to the appropriate receivers as determined by the handler.
          for (std::vector<ConnectionInfo>::iterator receiver=receivers.begin();
               receiver != receivers.end();
               receiver++) {
            send_message(sockfd, receiver->ip, receiver->port_number, data_out);
          }
        }
      }
      fflush(stdout);
    }

  }
}

