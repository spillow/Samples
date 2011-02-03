#ifndef _PEER_H_
#define _PEER_H_

#include "common.h"

#include "json/json.h"

#include <string>
#include <vector>
#include <map>
using namespace std;

class ConnectionInfo
{
public:
  ConnectionInfo() { ip = ""; port_number = -1; }
  ConnectionInfo(std::string ip, int port_number);
  std::string ip;
  int port_number;
};

struct CommunicationState
{
  //maps a peer_id to a map that maps a sequence_number to the information
  //of the peer that sent the query to you (not necessarily the originator,
  //just the neighbor that passed it along).
  std::map<int, std::map<int, ConnectionInfo> > past_received_queries_record;

  //keeps a list of all the neighbors who were found to have a file having
  //been notified by a query hit.
  std::map<std::string, std::vector<NeighborData> > who_has_what_file;

  //the socket communications are done on.
  int sockfd;

  bool query_in_progress;
  std::string file_queried_for;

  int search_timeout; //seconds

  //value to use on started queries.
  int ttl_value;
};

typedef bool (*StdinHandler)(std::vector<string> arguments,
                             std::string& data_out,
                             std::vector<ConnectionInfo>& receivers,
                             int timer_time_remaining,
                             int& set_new_time_remaining,
                             CommunicationState& comm_state,
                             SelfData& self_data);

typedef bool (*RecvHandler)(ConnectionInfo caller,
                            std::string data_in,
                            std::string& data_out,
                            std::vector<ConnectionInfo>& receivers,
                            int timer_time_remaining,
                            int& set_new_time_remaining,
                            CommunicationState& comm_state,
                            SelfData& self_data);

void main_event_loop(int sockfd,
                     std::map<std::string, StdinHandler> stdin_dispatcher,
                     std::map<std::string, RecvHandler> recv_dispatcher,
                     struct SelfData& self_data);

bool search_time_handler(std::vector<string> arguments,
                         std::string& data_out,
                         std::vector<ConnectionInfo>& receivers,
                         int timer_time_remaining,
                         int& set_new_time_remaining,
                         CommunicationState& comm_state,
                         SelfData& self_data);

bool help_handler(std::vector<string> arguments,
                  std::string& data_out,
                  std::vector<ConnectionInfo>& receivers,
                  int timer_time_remaining,
                  int& set_new_time_remaining,
                  CommunicationState& comm_state,
                  SelfData& self_data);

bool review_handler(std::vector<string> arguments,
                    std::string& data_out,
                    std::vector<ConnectionInfo>& receivers,
                    int timer_time_remaining,
                    int& set_new_time_remaining,
                    CommunicationState& comm_state,
                    SelfData& self_data);

bool progress_handler(std::vector<string> arguments,
                      std::string& data_out,
                      std::vector<ConnectionInfo>& receivers,
                      int timer_time_remaining,
                      int& set_new_time_remaining,
                      CommunicationState& comm_state,
                      SelfData& self_data);

bool listing_handler(std::vector<string> arguments,
                     std::string& data_out,
                     std::vector<ConnectionInfo>& receivers,
                     int timer_time_remaining,
                     int& set_new_time_remaining,
                     CommunicationState& comm_state,
                     SelfData& self_data);

bool neighbors_handler(std::vector<string> arguments,
                       std::string& data_out,
                       std::vector<ConnectionInfo>& receivers,
                       int timer_time_remaining,
                       int& set_new_time_remaining,
                       CommunicationState& comm_state,
                       SelfData& self_data);

bool query_handler(std::vector<string> arguments,
                   std::string& data_out,
                   std::vector<ConnectionInfo>& receivers,
                   int timer_time_remaining,
                   int& set_new_time_remaining,
                   CommunicationState& comm_state,
                   SelfData& self_data);

bool establish_neighbor_handler(ConnectionInfo caller,
                                std::string data_in,
                                std::string& data_out,
                                std::vector<ConnectionInfo>& receivers,
                                int timer_time_remaining,
                                int& set_new_time_remaining,
                                CommunicationState& comm_state,
                                SelfData& self_data);

bool queryhit_receive_handler(ConnectionInfo caller,
                              std::string data_in,
                              std::string& data_out,
                              std::vector<ConnectionInfo>& receivers,
                              int timer_time_remaining,
                              int& set_new_time_remaining,
                              CommunicationState& comm_state,
                              SelfData& self_data);

bool query_receive_handler(ConnectionInfo caller,
                           std::string data_in,
                           std::string& data_out,
                           std::vector<ConnectionInfo>& receivers,
                           int timer_time_remaining,
                           int& set_new_time_remaining,
                           CommunicationState& comm_state,
                           SelfData& self_data);

bool in_set_of_neighbors(std::string ip_address, int port_number,
                         int& peer_id, SelfData& self_data);

#endif //_PEER_H_
