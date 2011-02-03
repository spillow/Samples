package main

import (
  "fmt"
  "image"
  "image/png"
  "os"
  "sort"
  "math"
)

const (
  POSITIVE = 1
  NEGATIVE = 0
)

type HaarCorners image.Rectangle
type ImageDataArray []ImageData
type WeakClassifierArray []WeakClassifier

type WeakClassifier struct {
  feature  Feature
  polarity int
  thresh   int
  alpha    float
  error    float
}

func float_min(a, b float) float {
  if a < b {
    return a
  }

  return b
}

func (self WeakClassifier) classify(S [][]int) int {
  if self.polarity * self.feature.calc_feature_value(S) < self.polarity * self.thresh {
    return POSITIVE
  }

  return NEGATIVE
}

type StrongClassifier struct {
  weak_classifiers []WeakClassifier
  thresh           float
}

func (self StrongClassifier) classify(S [][]int) int {
  sum := 0.0
  for _,h := range self.weak_classifiers {
    sum += h.alpha * float(h.classify(S))
  }

  if sum >= self.thresh {
    return POSITIVE
  }

  return NEGATIVE
}

type ImageData struct {
  name     string
  class    int
  integral [][]int
  weight   float
  feature_value int
}

type Feature interface {
  calc_feature_value(S [][]int) int
}

type VertFeature struct {
  corners HaarCorners
}

type HorizFeature struct {
  corners HaarCorners
}

func (self HorizFeature) calc_feature_value(S [][]int) int {
  min := self.corners.Min
  max := self.corners.Max

  width  := (max.X - min.X) + 1

  x1,y1 := min.X,min.Y
  x3,y3 := min.X + width/2,min.Y
  x5,y5 := max.X + 1,min.Y
  x2,y2 := min.X,max.Y+1
  x4,y4 := min.X+width/2,max.Y+1
  x6,y6 := max.X+1,max.Y+1

  return weighted_sum_calc(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6, S)
}

func (self VertFeature) calc_feature_value(S [][]int) int {
  min := self.corners.Min
  max := self.corners.Max

  height := (max.Y - min.Y) + 1

  x1,y1 := min.X,min.Y
  x2,y2 := max.X+1,min.Y
  x3,y3 := min.X,min.Y + height/2
  x4,y4 := max.X+1,min.Y + height/2
  x5,y5 := min.X,max.Y + 1
  x6,y6 := max.X+1,max.Y+1

  return weighted_sum_calc(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6, S)
}

func weighted_sum_calc(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6 int, S [][]int) int {
  return -S[x1][y1] + S[x2][y2] + 2*S[x3][y3] - 2*S[x4][y4] - S[x5][y5] + S[x6][y6]
}

func sum_float_values(vals []float) float {
  cnt := 0.0
  for _,v := range vals {
    cnt += v
  }

  return cnt
}

func get_image_dims(img image.Image) (int, int) {
  img_rect := img.Bounds()
  width    := img_rect.Max.X - img_rect.Min.X
  height   := img_rect.Max.Y - img_rect.Min.Y

  return width,height
}

func convert_to_gray(img image.Image) image.Image {
  width,height := get_image_dims(img)

  gray_img := image.NewGray16(width, height)

  for x := 0; x < width; x++ {
    for y := 0; y < height; y++ {
      r,g,b,_ := img.At(x,y).RGBA()
      avg := (r + g + b) / 3.0
      gray_img.Set(x, y, image.Gray16Color{uint16(avg)})
    }
  }

  return gray_img
}

//up to but not including x
func get_img_row_sum(x, y int, img image.Image) int {
  sum := 0
  for i := 0; i < x; i++ {
    Y,_,_,_ := img.At(i, y).RGBA()
    sum += int(Y)
  }

  return sum
}

//up to but not including y
func get_img_col_sum(x, y int, img image.Image) int {
  sum := 0
  for i := 0; i < y; i++ {
    Y,_,_,_ := img.At(x, i).RGBA()
    sum += int(Y)
  }

  return sum
}

func calculate_image_integral(I image.Image) [][]int {
  width, height := get_image_dims(I)
  S := make([][]int, width+1)

  for i := 0; i < len(S); i++ {
    S[i] = make([]int, height+1)
  }

  for x := 1; x < len(S); x++ {
    for y := 1; y < len(S[x]); y++ {
      Y,_,_,_ := I.At(x-1,y-1).RGBA()
      S[x][y] = S[x-1][y-1] + get_img_row_sum(x-1,y-1,I) +
                      get_img_col_sum(x-1,y-1,I) + int(Y)
    }
  }

  return S
}

func read_images_into_memory(dirname string, class int) []ImageData {
  fp, err := os.Open(dirname, os.O_RDONLY, 0666)
  if err != nil {
    fmt.Println("error reading from " + dirname)
    os.Exit(1)
  }

  defer fp.Close()

  filenames, err := fp.Readdirnames(-1)

  if err != nil {
    fmt.Println("couldn't read names")
    os.Exit(1)
  }

  image_items := make([]ImageData, len(filenames))

  for idx,name := range filenames {
    full_path := dirname + name

    imagefp, err := os.Open(full_path, os.O_RDONLY, 0666)

    if err != nil {
      fmt.Println("error opening image at: " + full_path)
      os.Exit(1)
    }

    image, err := png.Decode(imagefp)
    imagefp.Close()

    if err != nil {
      fmt.Println("error decoding png: " + name)
      os.Exit(1)
    }

    gray_img := convert_to_gray(image)
    integral := calculate_image_integral(gray_img)
    image_items[idx] = ImageData{name: name, class: class, integral: integral}

    //fp,err := os.Open("test_dir/" + name, os.O_CREATE | os.O_WRONLY, 0777)
  }

  return image_items
}

func get_haar_like_horizontal_features(width, height int) []HorizFeature {
  features := []HorizFeature{}

  for w := 2; w <= width; w += 2 {
    for h := 1; h <= height; h++ {
      for hpos := 0; hpos + w <= width; hpos++ {
        for vpos := 0; vpos + h <= height; vpos++ {
          min_point := image.Point{X:hpos, Y:vpos}
          max_point := image.Point{X:hpos+(w-1), Y:vpos+(h-1)}
          rect := HaarCorners{Min: min_point, Max: max_point}
          features = append(features, HorizFeature{corners: rect})
        }
      }
    }
  }

  return features
}

func get_haar_like_vertical_features(width, height int) []VertFeature {
  features := []VertFeature{}

  for h := 2; h <= height; h += 2 {
    for w := 1; w <= width; w++ {
      for hpos := 0; hpos + w <= width; hpos++ {
        for vpos := 0; vpos + h <= height; vpos++ {
          min_point := image.Point{X:hpos, Y:vpos}
          max_point := image.Point{X:hpos+(w-1), Y:vpos+(h-1)}
          rect := HaarCorners{Min: min_point, Max: max_point}
          features = append(features, VertFeature{corners: rect})
        }
      }
    }
  }

  return features
}

func (self ImageDataArray) Len() int {
  return len(self)
}

func (self ImageDataArray) Less(i, j int) bool {
  if self[i].feature_value < self[j].feature_value {
    return true
  }

  return false
}

func (self ImageDataArray) Swap(i, j int) {
  self[i], self[j] = self[j], self[i]
}

func get_best_weak_classifier(examples []ImageData,
                              feature Feature) WeakClassifier {
  for i := 0; i < len(examples); i++ {
    examples[i].feature_value = feature.calc_feature_value(examples[i].integral)
  }

  //make a copy of the examples to sort
  ex_copy := make([]ImageData, len(examples))
  copy(ex_copy, examples)

  sort.Sort(ImageDataArray(ex_copy))

  //the total sum of positive example weights
  T_plus  := 0.0
  T_minus := 0.0
  for i := 0; i < len(ex_copy); i++ {
    if ex_copy[i].class == POSITIVE {
      T_plus  += ex_copy[i].weight
    } else {
      T_minus += ex_copy[i].weight
    }
  }

  S_minus := 0.0
  S_plus  := 0.0
  min_error := float(math.Inf(1))
  best_idx  := -1
  best_p    := 0
  for i := 0; i < len(ex_copy); i++ {
    neg_label := S_plus + (T_minus - S_minus)
    pos_label := S_minus + (T_plus - S_plus)
    error := float_min(neg_label, pos_label)
    if ex_copy[i].class == POSITIVE {
      S_plus  += ex_copy[i].weight
    } else {
      S_minus += ex_copy[i].weight
    }

    if error < min_error {
      best_idx  = i
      min_error = error
      if neg_label < pos_label {
        best_p = -1
      } else {
        best_p = 1
      }
    } else {
      best_idx = best_idx
    }
  }

  beta  := min_error / (1.0 - min_error)
  alpha := float(math.Log10(float64(1.0 / beta)))

  if best_idx < 0 {
    fmt.Println("index error for best index")
    best_idx = 0
  }

  best_weak_classifier :=  WeakClassifier{feature: feature, polarity: best_p,
                                          thresh: ex_copy[best_idx].feature_value,
                                          alpha: alpha,
                                          error: min_error}

  //update the weights
  for i := 0; i < len(examples); i++ {
    classification := best_weak_classifier.classify(examples[i].integral)
    e_i := -1
    if classification == examples[i].class {
      e_i = 0
    } else {
      e_i = 1
    }

    examples[i].weight = examples[i].weight * float(math.Pow(float64(beta), float64(1-e_i)))
  }

  return best_weak_classifier
}

func (self WeakClassifierArray) Len() int {
  return len(self)
}

func (self WeakClassifierArray) Less(i, j int) bool {
  if self[i].error < self[j].error {
    return true
  }

  return false
}

func (self WeakClassifierArray) Swap(i, j int) {
  self[i], self[j] = self[j], self[i]
}

func make_strong_classifier(pos_examples []ImageData,
                            neg_examples []ImageData,
                            all_examples []ImageData,
                            features []Feature) StrongClassifier {

  fmt.Println("making strong classifier...")
  weak_classifiers := make([]WeakClassifier, len(features))

  //initialize the weights for the first classifier
  num_pos_examples := len(pos_examples)
  num_neg_examples := len(neg_examples)
  for i := 0; i < len(all_examples); i++ {
    if all_examples[i].class == POSITIVE {
      all_examples[i].weight = 1.0 / float(2 * num_pos_examples)
    } else {
      all_examples[i].weight = 1.0 / float(2 * num_neg_examples)
    }
  }

  fmt.Println("generating weak classifiers")
  for t := 0; t < len(weak_classifiers); t++ {
    //normalize the weights
    classifier_sum := 0.0
    for _,q := range all_examples {
      classifier_sum += q.weight
    }

    for i := 0; i < len(all_examples); i++ {
      all_examples[i].weight /= classifier_sum
    }

    if t % 1000 == 0 {
      fmt.Println("running on t:", t)
    }

    //select the best weak classifier
    classifier := get_best_weak_classifier(all_examples, features[t])
    weak_classifiers[t] = classifier
  }

  //now sort the weak classifiers by their error
  sort.Sort(WeakClassifierArray(weak_classifiers))

  for i := 0; i < len(weak_classifiers); i++ {
    fmt.Println("running with", i+1, "classifiers")

    prune_at_least := 0.50
    stop_thresh    := 1e-6
    iter_limit     := 100

    //find an upper bound
    lower_bound := 0.0
    upper_bound := 0.0
    fmt.Println("finding upper thresh...")
    for thresh := stop_thresh; ; thresh *= 2.0 {
      strong_classifier := StrongClassifier{weak_classifiers: weak_classifiers[:i+1],
                                            thresh: thresh}
      if ! all_positives_allowed(strong_classifier, pos_examples) {
        upper_bound = thresh
        break
      }
    }

    fmt.Println("finding lower thresh...")
    for thresh := -stop_thresh; ; thresh *= 2.0 {
      strong_classifier := StrongClassifier{weak_classifiers: weak_classifiers[:i+1],
                                            thresh: thresh}
      if prune_negative_examples(strong_classifier, neg_examples) < prune_at_least {
        lower_bound = thresh
        break
      }
    }

    //fmt.Println("lower bound:", lower_bound)
    //fmt.Println("upper bound:", upper_bound)

    fmt.Println("binary search")
    curr_iter := 0
    for ;; {
      mid := (lower_bound + upper_bound) / 2.0

      curr_iter++
      if curr_iter > iter_limit {
        fmt.Println("iteration limit reached...")
        break
      }

      if (mid - lower_bound) < stop_thresh {
        strong_classifier := StrongClassifier{weak_classifiers: weak_classifiers[:i+1],
                                              thresh: lower_bound}

        if ! all_positives_allowed(strong_classifier, pos_examples) {
          break
        }

        cull_amount := prune_negative_examples(strong_classifier, neg_examples)
        fmt.Println("culled:", cull_amount)
        if cull_amount >= prune_at_least {
          return strong_classifier
        } else {
          break
        }
      } else {
        strong_classifier := StrongClassifier{weak_classifiers: weak_classifiers[:i+1],
                                              thresh: mid}
        if all_positives_allowed(strong_classifier, pos_examples) {
          lower_bound = mid
        } else {
          upper_bound = mid
        }
      }
    }
  }

  panic("couldn't find an appropriate strong classifier")
}

func prune_negative_examples(strong_classifier StrongClassifier,
                             negative_examples []ImageData) float {

  neg_cnt := 0
  for _,ex := range negative_examples {
    if strong_classifier.classify(ex.integral) == NEGATIVE {
      neg_cnt += 1
    }
  }

  removed := (float(neg_cnt) / float(len(negative_examples)))

  return removed
}

func all_positives_allowed(strong_classifier StrongClassifier, pos_examples []ImageData) bool {
  for _,ex := range pos_examples {
    if strong_classifier.classify(ex.integral) != ex.class {
      return false
    }
  }

  return true
}

func main() {
  images_train_root := "images/train/"
  images_test_root  := "images/test/"
  patch_width  := 40
  patch_height := 20

  fmt.Println("calculating haar feature shapes...")

  horiz_features := get_haar_like_horizontal_features(patch_width, patch_height)
  vert_features  := get_haar_like_vertical_features(patch_width, patch_height)

  fmt.Println("# horiz features: ", len(horiz_features))
  fmt.Println("# vert  features: ", len(vert_features))

  all_features := make([]Feature, len(horiz_features) + len(vert_features))
  for i := 0; i < len(horiz_features); i++ {
    all_features[i] = horiz_features[i]
  }

  for i,j := len(horiz_features), 0; j < len(vert_features); i,j = i+1,j+1 {
    all_features[i] = vert_features[j]
  }

  fmt.Println("getting image listing...")

  positive_examples := read_images_into_memory(images_train_root + "positive/", POSITIVE)
  negative_examples := read_images_into_memory(images_train_root + "negative/", NEGATIVE)

  all_examples := append(positive_examples, negative_examples...)

  strong_classifier_cascade := []StrongClassifier{}

  for k := 1; ; k++ {
    fmt.Println("stage:", k)
    curr_strong := make_strong_classifier(positive_examples, negative_examples, all_examples, all_features)
    strong_classifier_cascade = append(strong_classifier_cascade, curr_strong)

    //determine if we need to generate more strong classifiers
    remaining_neg_examples := []ImageData{}
    for _,neg_ex := range negative_examples {
      if curr_strong.classify(neg_ex.integral) == POSITIVE {
        remaining_neg_examples = append(remaining_neg_examples, neg_ex)
      }
    }

    /*if len(remaining_neg_examples) == 0 {
      break
    }*/

    if len(remaining_neg_examples) < 4 {
      break
    }

    fmt.Println("remaining negative examples:", len(remaining_neg_examples))

    negative_examples = remaining_neg_examples
    all_examples = append(positive_examples, negative_examples...)
  }

  fmt.Println("running cascaded classifier on test images...")

  //now run on the test data
  positive_test_examples := read_images_into_memory(images_test_root + "positive/", POSITIVE)
  negative_test_examples := read_images_into_memory(images_test_root + "negative/", NEGATIVE)

  all_test_examples := append(positive_test_examples, negative_test_examples...)

  for k,s_classifier := range strong_classifier_cascade {
    fmt.Println("stage:", k+1)

    remaining_test_examples := []ImageData{}
    for _,ex := range all_test_examples {
      if s_classifier.classify(ex.integral) == POSITIVE {
        remaining_test_examples = append(remaining_test_examples, ex)
      }
    }

    all_test_examples = remaining_test_examples

    //run through the result at the current point in the cascade and calculate the error rates
    curr_num_pos_rem := 0
    curr_num_neg_rem := 0
    for _,ex := range all_test_examples {
      if ex.class == POSITIVE {
        curr_num_pos_rem++
      } else {
        curr_num_neg_rem++
      }
    }

    fp := float(curr_num_neg_rem) / float(len(negative_test_examples))
    fn := float(len(positive_test_examples) - curr_num_pos_rem) / float(len(positive_test_examples))

    fmt.Println("fp rate:", fp)
    fmt.Println("fn rate:", fn)
  }
}

