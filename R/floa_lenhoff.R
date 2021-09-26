floa_lenhoff <- function(data, n.boot, cp.begin, alpha) {

  # Limit of Agreement calculation adapted from Lenhoff et al. (1999)
  # ----------------------------------------------------------------------------
  #
  # Default values:
  # Bootstrap iterations (n.boot)     : 100
  # Anfangswert Quantil (cp_begin)    : 0
  # Irrtumswahrscheinlichkeit (alpha) : 0.05
  # ----------------------------------------------------------------------------

  # Dimensions
  n.frames <- length(unique(data$frame))
  n.curves <- length(unique(data$strideID))

  # ----------------------------------------------------------------------------
  # Fourier Transformation
  #
  # Approximate time series (differences) using Fourier functions
  # ----------------------------------------------------------------------------

  # Number of basic (Fourier) functions
  # ... appears to plateau around 50 basis vectors
  k <- 50

  fd.basis <- fda::create.fourier.basis(nbasis = k)

  # Returns functional data representation of difference curves
  data.diff.fd <- fdaDelta(data, fd.basis)

  original.sample.fd <- data.diff.fd[[1]]
  original.sample.mean.fd <- data.diff.fd[[2]]
  original.sample.sd.fd <- data.diff.fd[[3]]

  # ----------------------------------------------------------------------------
  # Bootstrap
  # ----------------------------------------------------------------------------

  # for i=1:n_repeat
  # % neues Bootstrap Sample bilden
  # for k=1:size(daten,2)
  # bootstrap.zz(k,i)=floor(unifrnd(1,size(daten,2)+1));
  # bootstrap.pseudo_koeffi(:,k,i) = fourier.koeffi(:,bootstrap.zz(k,i));
  # bootstrap.real(:,k,i)=fourier.s*bootstrap.pseudo_koeffi(:,k,i);
  # end

  for (b in 1:n.boot) {
    # Extract information from class fd object
    # https://rdrr.io/cran/fda/src/R/plot.fd.R
    boot.sample <- sample(original.sample.fd, size = n.curves, replace = TRUE)

    # Matrix mit den (Fourier)Basisvektoren aufstellen
    # dim(basismatrix): [n.row, n.basisvectors]
    tvec <- seq(0, 100)
    mybasis <- fd.basis
    basismat = eval.basis(tvec, mybasis)
    # Koeffizientenmatrix
    coefmat <- original.sample.fd$coefs

    test <- basismat[1,] * coefmat[, 1] # Funktioniert noch nicht

    # # Gesampelt werden die Koeffizienten!?
    # curve.idx <- 1 # sample(...)
    # dim(coefmat)
    # tmp <- coefmat[, curve.idx]

    test <- basismat %*% coefmat
    # Einzelne Kurve samplen
    test <- basismat[, 1] * t(coefmat[1, ])
    test <- basismat %*% t(coefmat[1, ])

    # # Einzelne Kurve extrahieren
    # # plot(y, fdmat, type = "l")
    # y <- seq(0, 100)
    #
    # frame.idx <- 1 # darf kein Vektor sein
    # fdmat <- fda::eval.fd(frame.idx, original.sample.fd)
    # # eval.fd: basismat <- eval.basis(evalarg, basisobj, Lfdobj, returnMatrix)
    # # wval.basis: basismat <- getbasismatrix(evalarg, basisobj, nderiv, returnMatrix)
    # plot(fdmat[1,])

    boot.mean.fd <- fda::mean.fd(boot.sample)
    # Das ist möglicherweise die vereinfachte Variante der SD aus dem 'fda' package
    # Laut Paper braucht es die Kovarianzen für die Berechnung! (Sind die in der fda Version schon enthalten?)
    # Hier die Variante von Doris (Berechnung Kovarianzen):
    # for k=1:size(daten,2)
    #   bootstrap.std1(:,:,k)=(bootstrap.pseudo_koeffi(:,k,i)-bootstrap.mean(:,i))*...
    #     (bootstrap.pseudo_koeffi(:,k,i)-bootstrap.mean(:,i))';
    # end
    # bootstrap.kovarianz(:,:,i) = nanmean(bootstrap.std1,3);
    # bootstrap.std_all(:,:,i) = sqrt(fourier.s*bootstrap.kovarianz(:,:,i)*fourier.s');
    # for k=1:n_time
    #   bootstrap.std(k,i)=bootstrap.std_all(k,k,i);
    # end
    boot.sd.fd <- fda::std.fd(fd.diff)



  }

  # ----------------------------------------------------------------------------
  # Quantilbestimmung für zugehörige Irrtumswahrscheinlichkeit
  # ----------------------------------------------------------------------------

    # Bestimmung des Quantils für Irrtumswahrscheinlichkeit
    cp.mean <- 0
    cp.grenze <- cp_begin
    n <- 1

    cp.data   <- zeros(n.curves, n.boot)
    cp.data_i <- zeros(n.curves, n.boot)
    # cp.data   = zeros(n_kurven, n_repeat);
    # cp.data_i = zeros(n_kurven, n_repeat);

    while (cp.mean < (1 - alpha)) {
      for (i in 1:n.boot) {
        for (k in 1:n.frames) {
          cp.data(k,i) <- max(abs(fourier.real(:,k) - bootstrap.real_mw(:,i)) ./ bootstrap.std(:,i))
          cp.data_i(k,i) <- cp.data(k,i) < cp.grenze
        }
      }
    }
    cp.mean <- nanmean(mean(cp.data_i));
    cp.grenze <- cp.grenze+0.05;
    n <- n + 1
    }

    cp_out <- cp.grenze

    # ----------------------------------------------------------------------------
    # Prädiktionsband
    # ----------------------------------------------------------------------------
    # bootstrap_sample(:,1)=mean(bootstrap.real_mw,2);
    # bootstrap_sample(:,2)=mean(bootstrap.std,2);
    # bootstrap_sample(:,3)=bootstrap_sample(:,1)+cp.grenze*bootstrap_sample(:,2);
    # bootstrap_sample(:,4)=bootstrap_sample(:,1)-cp.grenze*bootstrap_sample(:,2);

  return(floa)
}
