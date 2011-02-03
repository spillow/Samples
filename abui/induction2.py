#!/usr/bin/env python

from __future__ import division
import random
import itertools

class Node:
  def __init__(self, value):
    self.value = value
    self.children = []

  def __repr__(self):
    return self.value.__repr__()

class ExprNode(Node):
  def __repr__(self):
    if self.value.__class__ == str:
      return '(' + self.value + ' ' + ' '.join([x.__repr__() for x in self.children]) + ')'
    elif self.value.__class__ == Feature:
      return self.value.__repr__()
    else:
      raise Exception('unknown value type in ExprNode')

def concat(l):
  first_flatten = []
  for item in l:
    first_flatten += item

  return first_flatten

def get_tree_leaves(root):
  if len(root.children) == 0:
    return [root]

  return concat([get_tree_leaves(child) for child in root.children])

def get_tree_post_order(root):
  if len(root.children) == 0:
    return [root]

  return concat([get_tree_post_order(child) for child in root.children]) + [root]

def argument_warranted(root):
  root_id = root.value[0]

  leaf_ids = [x.value[0] for x in get_tree_leaves(root)]

  return all(root_id == leaf_id for leaf_id in leaf_ids)

class Agent:
  def __init__(self, id, example_space, feature_pos_dict, tau_acceptability):
    print "Generating Agent:", id
    self.id = id
    self.example_space = example_space
    self.feature_pos_dict = feature_pos_dict
    self.tau_acceptability = tau_acceptability

    self.sent_arguments = []

    self.prev_hypothesis = None

    self.all_possible_rules = make_generalization_space(feature_pos_dict,example_space)

    self.most_general_term = max(self.all_possible_rules, key=lambda r: len(r.cover_set))

    self.hyp = generate_hypothesis(example_space, self.all_possible_rules,
                                   self.most_general_term, tau_acceptability, [])

  def receive_argument(self, argument, other_agent, protocol_state):
    if argument.__class__ == RuleArgument:

      attacker_gen = itertools.cycle([self, other_agent])
      attacked_gen = itertools.cycle([other_agent, self])
      curr_attacker = attacker_gen.next()
      curr_attacked = attacked_gen.next()

      root = Node((curr_attacked.id,argument))

      protocol_state.add_argument(argument, curr_attacked.id)

      while True:
        possible_nodes_to_attack = [x for x in get_tree_post_order(root) if x.value[0] != curr_attacker.id]

        found_attacking_argument = False
        for node_to_attack in possible_nodes_to_attack:
          curr_attacking_argument = ABUI(curr_attacker.example_space, node_to_attack.value[1].not_concept(),
                                         curr_attacker.get_Q(protocol_state), curr_attacker.all_possible_rules,
                                         node_to_attack.value[1].rule, curr_attacker.most_general_term,
                                         curr_attacker.tau_acceptability)

          if curr_attacking_argument is not None:
            found_attacking_argument = True
            break

        if not found_attacking_argument:
          break

        node_to_attack.children.append(Node((curr_attacker.id,curr_attacking_argument)))
        protocol_state.add_argument(curr_attacking_argument, curr_attacker.id)

        curr_attacker = attacker_gen.next()
        curr_attacked = attacked_gen.next()

      #traverse the tree and determine which arguments have been defeated
      for node in get_tree_post_order(root):
        if not argument_warranted(node):
          node.value[1].alive = False

      self.hyp = generate_hypothesis(self.example_space, self.all_possible_rules,
                                     self.most_general_term, self.tau_acceptability, self.get_Q(protocol_state))

    elif argument.__class__ == ExampleArgument:
      protocol_state.add_argument(argument, other_agent.id)
      self.example_space.append(argument.example)
      #update the cover of the rules
      for rule in self.all_possible_rules:
        if rule.covers(argument.example):
          rule.cover_set.add(argument.example)

      self.most_general_term = max(self.all_possible_rules, key=lambda r: len(r.cover_set))
      self.hyp = generate_hypothesis(self.example_space, self.all_possible_rules,
                                     self.most_general_term, self.tau_acceptability, self.get_Q(protocol_state))

      print "new hypothesis after example send"
      print self.hyp

    else:
      raise Exception('bad arg received')

  def get_own_arguments(self, protocol_state):
    own_args = []
    for (id, arg) in protocol_state.G_t[2]:
      if id == self.id:
        own_args.append(arg)

    return own_args

  def get_Q(self, protocol_state):
    undefeated_arguments = []
    for (_, arg) in protocol_state.G_t[2]:
      if arg.alive:
        undefeated_arguments.append(arg)

    return undefeated_arguments

  def get_I(self, protocol_state):
    i_args = []
    for (id, arg) in protocol_state.G_t[2]:
      if arg.alive:
        if id != self.id:
          if calc_confidence(arg, self.example_space) < self.tau_acceptability:
            i_args.append(arg)

    return i_args

class ProtocolState:
  def __init__(self):
    self.t = 0
    self.G_t = [[],[],[]]

  def add_argument(self, argument, id):
    self.G_t[2].append((id,argument))

  def bump_round(self):
    self.t += 1
    middle = self.G_t[1][:]
    self.G_t[1] = self.G_t[2][:]
    self.G_t[0] = middle

class RuleArgument:
  def __init__(self, rule, concept):
    self.rule = rule
    self.concept = concept

    self.alive = True

  def not_concept(self):
    if self.concept == '+':
      return '-'
    elif self.concept == '-':
      return '+'
    else:
      raise Exception("invalid concept")

  def __repr__(self):
    return "concept: " + self.concept + ", " + self.rule.__repr__()

class ExampleArgument:
  def __init__(self, example, concept):
    self.example = example
    self.concept = concept

    self.alive = True #can't beat example arguments

def make_argument(item, concept):
  """takes either a rule or example and makes an argument
     out of it with the target concept."""
  if item.__class__ == Rule:
    return RuleArgument(item, concept)
  elif item.__class__ == Example:
    return ExampleArgument(item, concept)
  else:
    raise Exception('attempting to make argument on unknown type')

class Hypothesis:
  """consists of a collections of rules that are
     or'd together to produce a result."""
  def __init__(self, rule_lst):
    self.rule_lst = rule_lst

  def __repr__(self):
    return ' or '.join(["'%s'" % (rep.__repr__()) for rep in self.rule_lst])

  def average_tau(self):
    return sum([calc_confidence(make_argument(rule, '+'), rule.example_space_seen)
                   for rule in self.rule_lst]) / len(self.rule_lst)

  def eval(self, example):
    result = False
    for rule in self.rule_lst:
      result = result or rule.eval(example)

    return result

  def covers(self, item):
    return any(rule.covers(item) for rule in self.rule_lst)

class Rule:
  def __init__(self, expr_tree, example_space):
    self.expr_tree = expr_tree

    self.example_space_seen = example_space

    self.cover_set = set()

    self.strict_cover_dict = {}
    self.eval_dict = {}

    self.update_cover(example_space)

  def equals_other(self, other):
    return self.cover_set == other.cover_set

  def __repr__(self):
    return "rule: " + self.expr_tree.__repr__()

  def eval_with_expr(self, expr_node, example):
    if expr_node.value.__class__ == str:
      if expr_node.value == 'and':
        return reduce(lambda acc,curr: acc and curr,
                      map(lambda x: self.eval_with_expr(x, example), expr_node.children), True)
      elif expr_node.value == 'not':
        return not self.eval_with_expr(expr_node.children[0], example)
      else:
        raise Exception('unknown logical op')
    elif expr_node.value.__class__ == Feature:
      for feature in example.feature_lst:
        if feature.feature == expr_node.value.feature:
          if feature.value == expr_node.value.value:
            return True
          else:
            return False

      return False
    else:
      raise Exception('unknown node in tree')

  def eval(self, example):
    if example in self.eval_dict:
      return self.eval_dict[example]

    result = self.eval_with_expr(self.expr_tree, example)
    self.eval_dict[example] = result

    return result

  def update_cover(self, example_space):
    """maintains an internal list of all the
       examples in the example space that the
       rule covers."""
    self.cover_set = set()
    for example in example_space:
      if self.eval(example):
        self.cover_set.add(example)

  def strict_cover(self, item):
    """determines whether the cover is strictly greater
       (doesn't matter for examples)."""

    if item in self.strict_cover_dict:
      return self.strict_cover_dict[item]

    if item.__class__   == Example:
      result = self.eval(item)
    elif item.__class__ == Rule:
      result = len(self.cover_set) > len(item.cover_set) and self.covers(item)
    else:
      raise Exception('attempting to strict cover unsupported type')

    self.strict_cover_dict[item] = result
    return result

  def covers(self, item):
    """determines whether the rule covers the given item.
       The item can be an example or another rule."""
    if item.__class__   == Example:
      return self.eval(item)
    elif item.__class__ == Rule:
      return self.cover_set.issuperset(item.cover_set)
    else:
      raise Exception('attempting to cover unsupported type')

class Feature:
  def __init__(self, feature, value):
    self.feature   = feature
    self.value     = value

  def __repr__(self):
    #return "feature: %s is %s" % (self.feature, self.value)
    return "%s is %s" % (self.feature, self.value)

class Example:
  """concept is either '+' or '-' and the feature list contains
     all of the features observed in the example (unknown features
     indicated by '?' in the test data are ignored."""
  def __init__(self, concept, feature_lst):
    self.concept = concept
    self.feature_lst = feature_lst

  def equals_other(self, other):
    if other.__class__ == Rule:
      return False
    elif other.__class__ == Example:
      return self.concept == other.concept and self.feature_lst == other.feature_lst
    else:
      raise Exception('unknown equality attempt on Example class')

  def __repr__(self):
    return "***example: type({concept}), features: {features}***".format(
                                        concept=self.concept,
                                        features=self.feature_lst)

def can_attack(argument, rule_argument):
  """returns whether the given argument (rule or example) can
     attack the given rule_argument (note that example arguments
     cannot be attacked."""
  if argument.__class__ == RuleArgument:
    return argument.concept == rule_argument.not_concept() and \
                rule_argument.rule.strict_cover(argument.rule)
  elif argument.__class__ == ExampleArgument:
    return argument.concept == rule_argument.not_concept() and \
                rule_argument.rule.covers(argument.example)
  else:
    raise Exception('passing in invalid type to can_attack()')

def calc_confidence(argument, example_space):
  """calculates the confidence level of a rule or example
     argument."""
  if argument.__class__ == ExampleArgument:
    return 1 #we have complete confidence in examples
  elif argument.__class__ == RuleArgument:
    numer = [e for e in example_space if e.concept == argument.concept
                                and argument.rule.covers(e)]
    denom = [e for e in example_space if argument.rule.covers(e)]

    num = (len(numer) + 1) / (len(denom) + 2)
    #print "doin the div: (%d + 1) / (%d + 2)" % (len(numer), len(denom))
    return num
  else:
    raise Exception("confidence calc on unknown type")

def get_feature_names(feature_file):
  """grabs the names of all the recorded possible
   features."""
  features = []
  with open(feature_file, 'r') as f:
    for line in f.readlines():
      features.append(line.split(':')[0])

  return features

def get_feature_possibilities(feature_file):
  """returns a dict mapping feature names to
     a list of possible values it can take."""
  feature_dict = {}
  with open(feature_file, 'r') as f:
    for line in f.readlines():
      (feature, possibilities) = line.split(':')
      feature_dict[feature.strip()] = [x.strip() for x in possibilities.split(',')]

  return feature_dict

def make_generalization_space(feature_dict, example_space):
  """generates a list of all possible rules that
     can occur."""
  rules = []

  #first generate all of the not based trees
  for (feature,possibilities) in feature_dict.iteritems():
    for poss in possibilities:
      curr_node = ExprNode('not')
      curr_node.children = [ExprNode(Feature(feature,poss))]
      rules.append(Rule(curr_node, example_space))

  #now the and type trees
  for (feature1,possibilities1) in feature_dict.iteritems():
    for poss1 in possibilities1:
      for (feature2,possibilities2) in feature_dict.iteritems():
        for poss2 in possibilities2:
          curr_node = ExprNode('and')
          curr_node.children = [ExprNode(Feature(feature1,poss1)), ExprNode(Feature(feature2, poss2))]
          rules.append(Rule(curr_node, example_space))

  return rules

def get_all_classes(data_file):
  """returns a list of all the possible types of classes
     (specifically, of damaged soybean in this case)."""
  classes = []
  with open(data_file, 'r') as f:
    for line in f.readlines():
      classes.append(line.split(',')[0])

  return list(set(classes))

def generate_hypothesis(example_space, generalization_space,
                        most_general_term, tau_acceptability, argument_lst):
  """creates a hypothesis consisting of the logical disjunction of a set of
     rules meant to cover positive examples.  This is used in the initial phase
     to creating a starting hypothesis for each agent and it is also used in
     the centralized method of induction when all of the example space is used."""
  rule_lst = []
  curr_examples = example_space
  running_cover = [] #all the examples currently covered
  while len(curr_examples) != 0:
    curr_rule = ABUI(curr_examples, '+', argument_lst, generalization_space,
                     most_general_term, most_general_term, tau_acceptability)

    if curr_rule is None: #finished
      break

    rule_lst.append(curr_rule)
    print '-----'
    print calc_confidence(make_argument(curr_rule, '+'), example_space)
    print calc_confidence(make_argument(curr_rule, '+'), curr_examples)
    print '-----'

    #find the set of positive examples still not covered.
    running_cover += list(curr_rule.cover_set)
    curr_examples = list(set(example_space) - set(running_cover))

  return Hypothesis(rule_lst)

def ABUI(example_space, concept, argument_lst, generalization_space,
         generalization, most_general_term, tau_acceptability):
  """Argumentation-based Bottom-up Induction algorithm.  Can be
     used to generate new rules to build up a hypothesis or new
     rule arguments for attacking other rule arguments."""
  H = []
  for example in [e_ for e_ in example_space if e_.concept==concept and
                        generalization.covers(e_)]:
    c = example
    while not c.equals_other(most_general_term):
      if calc_confidence(make_argument(c, concept), example_space) >= tau_acceptability:
        H.append(c)

      G  = gamma_fn(c, generalization_space)
      G_ = [h for h in G if generalization.strict_cover(h) and
             not any(can_attack(alpha,make_argument(h,concept)) for alpha in argument_lst)]

      if len(G_) == 0:
        c = most_general_term
      else:
        c = argmax(lambda v: calc_confidence(v, example_space),
                        [make_argument(x, concept) for x in G_]).rule

  if len([x for x in H if x.__class__ == Rule]) == 0:
    return None
  else:
    return argmax(lambda v: calc_confidence(v, example_space),
                      [make_argument(x, concept) for x in H if x.__class__ == Rule]).rule

def argmax(f, coll):
  """returns the item in the collection that maximizes f."""
  return max(coll, key=lambda item: f(item))

def gamma_fn(item, generalization_space):
  """takes an item as a rule or example and returns
     all the generalization refinements."""

  #print "running gamma fn"
  possible_generalizations = []
  for gen in generalization_space:
    if gen.strict_cover(item):
      possible_generalizations.append(gen)

  #print "getting refinements"

  gen_refinement = [x for x in possible_generalizations if
                    not any(x.strict_cover(other) for other in possible_generalizations)]

  #print "done with refinements"

  return gen_refinement

def generate_examples(data_file, feature_name_lst, feature_pos_dict, target_class):
  """generates a list of all examples in the example space from the file with the
     target_class denoted as the '+' class and the rest of the classes as '-'."""
  example_lst = []
  with open(data_file, 'r') as f:
    for line in f.readlines():
      feature_lst = []
      line_split = line.split(',')
      (class_name, numbers) = line_split[0],line_split[1:]
      for (idx,num) in enumerate(numbers):
        try:
          feature_idx = int(num)
        except ValueError:
          continue

        curr_feature  = feature_name_lst[idx]
        possibilities = feature_pos_dict[curr_feature]
        curr_value    = possibilities[feature_idx]

        feature_lst.append(Feature(feature=curr_feature,value=curr_value))

      example_lst.append(Example('+' if class_name==target_class else '-', feature_lst))

  return example_lst

def run_hypothesis_on_test_data(test_data_file, feature_name_lst, feature_pos_dict,
                                target_class, hypothesis):
  """runs the examples from the test data through the hypothesis generated from
     the training data and prints out statistics."""
  example_space = generate_examples(test_data_file, feature_name_lst,
                                    feature_pos_dict, target_class)

  correct_cnt   = 0
  neg_cnt       = 0
  pos_cnt       = 0
  false_pos_cnt = 0
  false_neg_cnt = 0
  for example in example_space:
    result = hypothesis.eval(example)
    if result == True and example.concept == '+':
      correct_cnt += 1
    elif result == False and example.concept == '-':
      correct_cnt += 1

    if example.concept == '-':
      neg_cnt += 1

      if result == True:
        false_pos_cnt += 1

    if example.concept == '+':
      pos_cnt += 1

      if result == False:
        false_neg_cnt += 1

  percent_correct = (correct_cnt / len(example_space))
  print "results:"
  print "%d / %d = %.1f%% correct" % (correct_cnt,len(example_space),percent_correct*100)

  avg_tau = hypothesis.average_tau()
  false_positive = false_pos_cnt / neg_cnt
  false_negative = false_neg_cnt / pos_cnt

  print "average tau:", hypothesis.average_tau()
  print "false positive:", false_positive
  print "false negative:", false_negative

  return percent_correct,avg_tau,false_positive,false_negative

def A_MAIL(agent_1, agent_2):
  protocol_state = ProtocolState()
  token_gen = itertools.cycle([agent_1, agent_2])
  token_other_gen = itertools.cycle([agent_2, agent_1])

  #initiate protocol state by adding hypothesis rules to G_t
  for rule in agent_1.hyp.rule_lst:
    agent_2.receive_argument(make_argument(rule, '+'), agent_1, protocol_state)

  for rule in agent_2.hyp.rule_lst:
    agent_1.receive_argument(make_argument(rule, '+'), agent_2, protocol_state)

  curr_agent  = token_gen.next()
  other_agent = token_gen.next()

  while True:
    if curr_agent.prev_hypothesis is not curr_agent.hyp:
      print "hyp not same for agent:", curr_agent.id
      for rule in curr_agent.hyp.rule_lst:
        arg_to_send = make_argument(rule, '+')
        if arg_to_send not in curr_agent.sent_arguments:
          curr_agent.sent_arguments.append(arg_to_send)
          other_agent.receive_argument(arg_to_send, curr_agent, protocol_state)

      curr_agent.prev_hypothesis = curr_agent.hyp

    #try to send an attacking argument to the other agent
    found_arg_for_attack = False
    for possible_arg_to_attack in curr_agent.get_I(protocol_state):
      print "trying to attack:", possible_arg_to_attack
      curr_attacking_argument = ABUI(curr_agent.example_space, possible_arg_to_attack.not_concept(),
                                     curr_agent.get_Q(protocol_state), curr_agent.all_possible_rules,
                                     possible_arg_to_attack.rule, curr_agent.most_general_term,
                                     curr_agent.tau_acceptability)

      if curr_attacking_argument is not None:
        if curr_attacking_argument not in curr_agent.sent_arguments:
          curr_agent.sent_arguments.append(curr_attacking_argument)
          print "found an attack arg beta"
          other_agent.receive_argument(curr_attacking_argument, curr_agent, protocol_state)
          protocol_state.bump_round()
          curr_agent  = token_gen.next()
          other_agent = token_gen.next()
          found_arg_for_attack = True
          break

    if found_arg_for_attack:
      continue

    #look for example attacks
    found_example_for_attack = False
    print "looking for examples..."
    for example in curr_agent.example_space:
      if (example.concept == '+' and not other_agent.hyp.covers(example)): #or \
         #(example.concept == '-' and other_agent.hyp.covers(example)):
        arg_to_send = make_argument(example, example.concept)
        if arg_to_send not in curr_agent.sent_arguments:
          curr_agent.sent_arguments.append(arg_to_send)
          print "found example to send"
          other_agent.receive_argument(arg_to_send, curr_agent, protocol_state)
          protocol_state.bump_round()
          curr_agent  = token_gen.next()
          other_agent = token_gen.next()
          found_example_for_attack = True
          break

    if found_example_for_attack:
      continue

    print "checking G_t length...", len(protocol_state.G_t[0]), len(protocol_state.G_t[2])
    if len(protocol_state.G_t[0]) == len(protocol_state.G_t[2]): #done
      return agent_1.hyp, agent_2.hyp
    else:
      protocol_state.bump_round()
      curr_agent  = token_gen.next()
      other_agent = token_gen.next()
      continue

def main():
  feature_file       = 'data/feature-names.txt'
  training_data_file = 'data/soybean-large.data'
  test_data_file     = 'data/soybean-large.test'
  tau_acceptability  = 0.65

  feature_name_lst   = get_feature_names(feature_file)
  feature_pos_dict   = get_feature_possibilities(feature_file)
  all_classes_lst    = get_all_classes(training_data_file)

  centralized_avg_tau_vals   = []
  centralized_false_pos_vals = []
  centralized_false_neg_vals = []
  centralized_acc_pos_vals   = []

  for curr_class in all_classes_lst:
    example_space = generate_examples(training_data_file, feature_name_lst,
                                    feature_pos_dict, curr_class)

    print "working on class:", curr_class

    all_possible_rules = make_generalization_space(feature_pos_dict,example_space)

    print "num possible rules:", len(all_possible_rules)

    #find the most general term in the set of all possible rules (it is the one
    #that covers more examples than any other).
    most_general_term = max(all_possible_rules, key=lambda r: len(r.cover_set))

    print "***Centralized Data Run***"

    hyp = generate_hypothesis(example_space, all_possible_rules, most_general_term, tau_acceptability, [])

    if len(hyp.rule_lst) == 0:
      print "***failed to generate hypothesis***"
    else:
      print hyp
      correct,tau,fp,fn = run_hypothesis_on_test_data(test_data_file, feature_name_lst, feature_pos_dict,
                                  curr_class, hyp)
      centralized_acc_pos_vals.append(correct)
      centralized_avg_tau_vals.append(tau)
      centralized_false_pos_vals.append(fp)
      centralized_false_neg_vals.append(fn)

    continue

    print "***A-MAIL Run***"

    random.shuffle(example_space)
    mid_line = int(len(example_space) / 2)

    agent_1 = Agent(1, example_space[:mid_line], feature_pos_dict, tau_acceptability)
    agent_2 = Agent(2, example_space[mid_line:], feature_pos_dict, tau_acceptability)

    print "Agent 1 starting hypothesis:", agent_1.hyp
    print "Agent 2 starting hypothesis:", agent_2.hyp

    a_mail_hyp_1, a_mail_hyp_2 = A_MAIL(agent_1, agent_2)

    if len(a_mail_hyp_1.rule_lst) == 0:
      print "***failed to generate hypothesis for A-MAIL***"
    else:
      print "Final Agent 1:", a_mail_hyp_1
      print "Final Agent 2:", a_mail_hyp_2
      run_hypothesis_on_test_data(test_data_file, feature_name_lst, feature_pos_dict,
                                  curr_class, a_mail_hyp_1)

  print "Centralized Average Accuracy:", sum(centralized_acc_pos_vals) / len(centralized_acc_pos_vals)
  print "Centralized Average Tau:", sum(centralized_avg_tau_vals) / len(centralized_avg_tau_vals)
  print "Centralized Average False-Positive:", sum(centralized_false_pos_vals) / len(centralized_false_pos_vals)
  print "Centralized Average False-Negative:", sum(centralized_false_neg_vals) / len(centralized_false_neg_vals)

if __name__ == '__main__':
  main()

