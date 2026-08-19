"""Plain nn.Module mirrors of the Lightning modules trained in DEEP_PATIENT.ipynb.

Attribute names (`encoder`, `decoder`, `model`) match the training classes so the
exported state_dicts load with strict=True.
"""
from __future__ import annotations

import torch.nn as nn


class DenoisingAutoencoder(nn.Module):
    def __init__(self, input_dim: int, bottleneck_dim: int = 512):
        super().__init__()
        self.encoder = nn.Sequential(
            nn.Linear(input_dim, 256), nn.ReLU(),
            nn.Linear(256, 128), nn.ReLU(),
            nn.Linear(128, bottleneck_dim),
        )
        self.decoder = nn.Sequential(
            nn.Linear(bottleneck_dim, 128), nn.ReLU(),
            nn.Linear(128, 256), nn.ReLU(),
            nn.Linear(256, input_dim),
        )

    def forward(self, x):
        z = self.encoder(x)
        return self.decoder(z), z


class MLPRegressor(nn.Module):
    def __init__(self, input_dim: int, hidden_dims: tuple = (512, 256, 128), dropout: float = 0.2):
        super().__init__()
        layers: list[nn.Module] = []
        prev = input_dim
        for h in hidden_dims:
            layers += [nn.Linear(prev, h), nn.ReLU(), nn.Dropout(dropout)]
            prev = h
        layers.append(nn.Linear(prev, 1))
        self.model = nn.Sequential(*layers)

    def forward(self, x):
        return self.model(x)
