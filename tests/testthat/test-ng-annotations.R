test_that("neuroglancer annotations work", {
  skip_if_not_installed('purrr')
  xyz=matrix(rnorm(18), ncol=3)

  # layers with colour palette
  df=data.frame(point=xyzmatrix2str(xyz),
                layer=rep(LETTERS[1:3], 2))
  colpal=c(A="red", C="green", B="blue")
  expect_is(ann <- ngl_annotation_layers(df, rawcoords=T, colpal=colpal),
            'list')


  expect_is(sc <- ngl_blank_scene()+ann, 'ngscene')
  expect_is(annback <- ngl_annotations(sc), 'data.frame')
  expect_equal(ngl_annotations(ann), annback)
  expect_equal(sapply(sc$layers[LETTERS[1:3]], "[[", "annotationColor"),
               col2hex(colpal[LETTERS[1:3]]))

  # colours and layers
  df=data.frame(point=xyzmatrix2str(xyz),
                col=rep(c("red", "blue", "green"), 2),
                layer=rep(LETTERS[1:3], 2))
  expect_is(ann2 <- ngl_annotation_layers(df, rawcoords=T), 'list')

  # little function to remove (nested) list elements by name
  lremove = function(l, toremove){
    m = names(l) %in% toremove
    l = if(any(m)) l[!m] else l
    if(is.list(l)) sapply(l, lremove, toremove, simplify = FALSE)
    else l
  }

  expect_equal(lremove(ann, "id"),
               lremove(ann2, "id"))

  # colours but no layers
  df2=data.frame(point=xyzmatrix2str(xyz),
                col=rep(c("red", "blue", "green"), 2))
  expect_equal(ngl_annotations(ngl_annotation_layers(df2))$layer,
               c("annotation 1", "annotation 1", "annotation 2", "annotation 2",
                 "annotation 3", "annotation 3"))
})

