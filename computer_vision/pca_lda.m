1; % Scott Pillow (run in octave)

clear

training_data_path = "images/train/";
test_data_path     = "images/test/";

function ret = get_grayscale_vector_from_file(file_path)
  [img_data] = imread(file_path);
  [gray_img] = rgb2gray(img_data);

  ret = double(reshape(gray_img, [], 1));
end

function filenames = get_images_at_dir(folder)
  file_structs = dir([folder, "/*.png"]);

  for i = 1:rows(file_structs)
    filenames{i} = file_structs(i).name;
  end
end

%these are the feature vectors as columns; no mean subtraction yet.
function ret = make_matrix_of_feature_vectors(root_folder)
  filenames = get_images_at_dir(root_folder);

  for i = 1:length(filenames)
    full_path = [root_folder, filenames{i}];
    ret(:,i) = get_grayscale_vector_from_file(full_path);
  end
end

function m = column_vector_mean(feature_matrix)
  m = mean(feature_matrix, 2);
end

%this is the big X value
function ret = get_mean_subtracted_matrix(feature_matrix)
  m = column_vector_mean(feature_matrix);
  rep_mean = m(:,ones(1,columns(feature_matrix)));
  ret = feature_matrix - rep_mean;
end

%uses a computational trick to relate the eigenvectors of X^TX
%to XX^T.
function [w_norm, D] = get_eigenvector_matrix(X)
  [u, D] = eigs(X'*X, columns(X)-3);
  w = X * u;

  %we must normalize the resulting eigenvectors
  w_norm = normalize_columns(w);
end

%retains only the k largest eigenvectors.  Eigenvectors are stored
%from left to right in descending order of eigenvalue magnitude.
function ret = retain_k_eigenvectors(eigenvector_matrix, k)
  ret = eigenvector_matrix(:,1:k);
end

%returns a k dimensional column vector
function ret = project_to_lower_dim(w_k, x, m, filename)
  point = w_k' * (x - m);
  ret = struct("point", point, "name", filename);
end

%extracts the front number from the image name to determine who it is.
function ret = get_subject_number_from_image_name(image_name)
  ret = str2num(image_name(1:2));
end

function ret = distance(p1, p2)
  diff = p1 - p2;
  ret = sqrt(diff' * diff);
end

function closest_neighbor = nearest_neighbor(dim_space, new_point)
  curr_min_dist = distance(dim_space(1).point, new_point.point);
  closest_neighbor = dim_space(1);
  for i = 2:length(dim_space)
    curr_dist = distance(dim_space(i).point, new_point.point);
    if curr_dist < curr_min_dist
      closest_neighbor = dim_space(i);
      curr_min_dist = curr_dist;
    end
  end
end

function B = normalize_columns(A)
  scale_values = 1 ./ norm(A, "columns");
  for i = 1:length(scale_values)
    B(:,i) = A(:,i) * scale_values(i);
  end
end

%runs pca classification on with specified training and test data using a
%dimensionality of p.
function accuracy = run_pca(training_path, test_path, p)
  disp("getting feature vectors from training files")
  training_feature_vectors = make_matrix_of_feature_vectors(training_path);
  m = column_vector_mean(training_feature_vectors);
  X = get_mean_subtracted_matrix(training_feature_vectors);
  disp("calculating eigenvector matrix using trick")
  [w,_] = get_eigenvector_matrix(X);

  training_image_names = get_images_at_dir(training_path);

  %now run through the test data and project
  disp("getting feature vectors from testing files")
  test_feature_vectors = make_matrix_of_feature_vectors(test_path);
  test_image_names = get_images_at_dir(test_path);

  for p_cnt = 1:length(p)
    printf("running p = %d\n", p(p_cnt))
    w_k = retain_k_eigenvectors(w, p(p_cnt));

    for i = 1:length(training_image_names)
      lower_dim_space(i) = project_to_lower_dim(w_k, training_feature_vectors(:,i),
                                                m, training_image_names{i});
    end

    correct_cnt = 0;
    for i = 1:length(test_image_names)
      test_projected = project_to_lower_dim(w_k, test_feature_vectors(:,i), m, test_image_names{i});
      closest_neighbor = nearest_neighbor(lower_dim_space, test_projected);

      %printf("%s -> %s\n", test_image_names{i}, closest_neighbor.name);

      if get_subject_number_from_image_name(test_image_names{i}) == ...
         get_subject_number_from_image_name(closest_neighbor.name)
        correct_cnt += 1;
      end
    end

    accuracy(p_cnt) = correct_cnt / length(test_image_names);
    printf("accuracy = %f\n", accuracy(p_cnt))
  end
end

%%%%%%%%%%%%%%  LDA  %%%%%%%%%%%%%%%%%%%

function fv = get_feature_vectors_by_class(images_dir)
  filenames = get_images_at_dir(images_dir);

  for i = 1:30
    fv{i} = [];
  end

  for i = 1:length(filenames)
    full_path = [images_dir, filenames{i}];
    subject_num = get_subject_number_from_image_name(filenames{i});

    curr_num_cols = columns(fv{subject_num});
    fv{subject_num}(:,curr_num_cols+1) = get_grayscale_vector_from_file(full_path);
  end
end

function ret = get_mean_vector_matrix(class_feature_vectors)
  for i = 1:length(class_feature_vectors)
    ret(:,i) = column_vector_mean(class_feature_vectors{i});
  end
end

function W_norm = yu_yang(class_feature_vectors)
  num_classes = length(class_feature_vectors);
  class_means = get_mean_vector_matrix(class_feature_vectors);
  global_mean = column_vector_mean(class_means);

  eigen_thresh = 0.1;

  %construct matrix for S_b to do computation trick
  X_sb = get_mean_subtracted_matrix(class_means);

  [V, L] = get_eigenvector_matrix(X_sb);
  Y = [];
  for i = 1:columns(V)
    if abs(L(i,i)) > eigen_thresh
      Y(:,columns(Y)+1) = V(:,i);
    end
  end

  D_b = L(1:columns(Y), 1:columns(Y));

  Z = Y * sqrtm(inv(D_b));

  %first put S_w into GG^T form:
  X_sw = [];
  for i = 1:num_classes
    for j = 1:columns(class_feature_vectors{i})
      X_sw(:,columns(X_sw)+1) = class_feature_vectors{i}(:,j) - class_means(i);
    end
  end

  [U, L_w] = get_eigenvector_matrix(Z' * X_sw);

  %W = U' * Z';
  W = Z * U;
  W_norm = normalize_columns(W);

  %only will keep the rightmost columns of W (the eigenvectors with smallest eigenvalues).
end

function accuracy = run_lda(training_path, test_path, p)
  disp("grabbing features by class")
  class_feature_vectors = get_feature_vectors_by_class(training_path);

  disp("calculating W via yu yang")
  W = yu_yang(class_feature_vectors);

  class_means = get_mean_vector_matrix(class_feature_vectors);
  global_mean = column_vector_mean(class_means);

  disp("grabbing training feature vectors")
  training_feature_vectors = make_matrix_of_feature_vectors(training_path);
  training_image_names = get_images_at_dir(training_path);

  disp("grabbing test feature vectors")
  test_feature_vectors = make_matrix_of_feature_vectors(test_path);
  test_image_names = get_images_at_dir(test_path);

  for p_cnt = 1:length(p)
    printf("running p = %d\n", p(p_cnt))
    w_k = retain_k_eigenvectors(reverse_columns(W), p(p_cnt));

    for i = 1:length(training_image_names)
      lower_dim_space(i) = project_to_lower_dim(w_k, training_feature_vectors(:,i),
                                                global_mean, training_image_names{i});
    end

    correct_cnt = 0;
    for i = 1:length(test_image_names)
      test_projected = project_to_lower_dim(w_k, test_feature_vectors(:,i), global_mean, test_image_names{i});
      closest_neighbor = nearest_neighbor(lower_dim_space, test_projected);

      %printf("%s -> %s\n", test_image_names{i}, closest_neighbor.name);

      if get_subject_number_from_image_name(test_image_names{i}) == ...
         get_subject_number_from_image_name(closest_neighbor.name)
        correct_cnt += 1;
      end
    end

    accuracy(p_cnt) = correct_cnt / length(test_image_names);
    printf("accuracy = %f\n", accuracy(p_cnt))
  end
end

function ret = reverse_columns(A)
  i = 1;
  j = columns(A);
  ret = A;
  while i < j
    tmp = A(:,i);
    ret(:,i) = A(:,j);
    ret(:,j) = tmp;
    i += 1;
    j -= 1;
  end
end

function graph_pca_and_lda(training_path, test_path, max_eigen_retained)
  acc_vals_pca = run_pca(training_path, test_path, 1:max_eigen_retained);
  acc_vals_lda = run_lda(training_path, test_path, 1:max_eigen_retained);

  disp(acc_vals_pca)
  disp(acc_vals_lda)

  plot(1:max_eigen_retained, acc_vals_pca, "-.", 1:max_eigen_retained, acc_vals_lda, "-.")
  xlabel("# of eigenvectors retained")
  ylabel("classification accuracy")
  title("Comparison of PCA and LDA")
  legend("PCA", "LDA", "location", "South")
  grid on
end

